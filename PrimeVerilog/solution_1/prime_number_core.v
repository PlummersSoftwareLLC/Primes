`timescale 1ns / 1ps

/*
This is the main module for prime sieve. The max prime that can be requested is bounded by the
width of the address bus and the ram array backing it. This is controlled by parameter PRIME_BITS,
which defaults to 20. This allows a little over a million as the max prime. 
When instantiating you can pass a different PRIME_BITS value, and it will flow through to the storage
module (see way below). For every extra bit, the max prime doubles, but the size of the ram storage in BRAM 
doubles too. At some point the synthesizer will have to add so many extra address decoding stages that
the latency/propagation delay would go up, and then the max frequency would go down.

basic idea is 
setup: 
    set we_i high
    set dat_i to max primes (less than 2**PRIME_BITS)
    set stb_i high for one clock cycle
    set stb_i back to low
    
    stall_o will go high until the primes have been calculated
    wait until stall_o goes low
    
to check if a number is bit_data
    set we_i low
    set address_i to the integer you want to know prime'ness on
    set stb_i high for one clock cycle
    set stb_i low
    
    read dat_o 
*/
module prime_number_core
#(parameter PRIME_BITS = 20) // default 20, but can be changed in vivado in IP Integrator by configuring the IP
( // naming convention on external pins is wishbone
// the comments in parenthesis are pragmas to Vivado so it knows how to auto-connect the clock and reset lines. in IP Integrator
    (* X_INTERFACE_INFO = "xilinx.com:signal:clock:1.0 clk_i CLK" *)
    (* X_INTERFACE_PARAMETER = "ASSOCIATED_RESET rst_i, FREQ_HZ 100000000" *)
    input clk_i,
    input [PRIME_BITS-1:0] address_i,
    input [PRIME_BITS-1:0] dat_i,
    output dat_o, /* just one bit */
    (* X_INTERFACE_INFO = "xilinx.com:signal:reset:1.0 rst_i RST" *)
    (* X_INTERFACE_PARAMETER = "POLARITY ACTIVE_HIGH" *)
    input rst_i,
    output ack_o,
    input stb_i,
    input we_i,
    output stall_o
);

    reg busy = 1'b0; // ack_o
    reg ack = 1'b0; // ack_o
    reg [PRIME_BITS-1:0] prime; // the prime number we are multiplying to get the composite numbers
    reg [PRIME_BITS-1:0] multiple_number; // which multiple of prime are we setting as non-prime. optimized away in synthesis, but useful for debugging simulation. 
    wire stored_is_prime; // is the number presented in next_prime or address_i prime as far as we know, register is inside prime_storage, this is a tap so we can see the value
    reg [PRIME_BITS-1:0] max_prime; // we wanted primes up to this number
    reg [PRIME_BITS-2:0] set_addr; // which bit in storage are we writing to (nth odd number)
    reg set_val = 0; // what value are we writing into the storage bit
    reg set_enable = 0; // are we actively writing bits to storage
    // FSM states, one hot encoding
    localparam FSM_SIZE = 4;
    reg [FSM_SIZE-1:0] state;
    localparam IDLE  = 4'b0001,
    RESET_ALL_TO_PRIME = 4'b0010,
    RUN_SIEVE = 4'b0100,
    RUN_SIEVE_LOOP = 4'b1000;


    storage #(.STORAGE_BITS(PRIME_BITS-1)) prime_storage ( // note storage uses one less bit, because we only store odd number prime'ness
        .clk_i(clk_i),
        .r_address_i(busy? next_prime[PRIME_BITS-1:1] : address_i[PRIME_BITS-1:1]),
        .r_bit(stored_is_prime),
        .w_address_i(set_addr),
        .w_bit(set_val),
        .we_i(set_enable)
    );
    
    wire done;
    wire next_prime_stall;
    wire [PRIME_BITS-1:0] next_prime;
    wire [PRIME_BITS-1:0] next_prime_sqr;
    reg request_next_prime;

    find_next_prime #(.PRIME_BITS(PRIME_BITS)) next_prime_finder(
        .clk_i (clk_i),
        .rst_i (rst_i|| state == IDLE || state == RESET_ALL_TO_PRIME),
        .request_next_prime_i(request_next_prime),
        .stored_is_prime_i(stored_is_prime),
        .max_prime_i(max_prime),    
        .next_prime_o(next_prime),
        .next_prime_sqr_o(next_prime_sqr),
        .stall_o(next_prime_stall), // when high, we haven't picked a prime yet, calling module should wait
        .done_o(done) // when high, the next prime sqared is greater than max prime
    );
    

    always @ (posedge clk_i)
    begin
        if (rst_i) begin
            // Reset logic
            state <= IDLE;
            busy <= 1'b0;
            ack <=1'b0;
            request_next_prime<=0;
        end else if ( (stb_i) && (!stall_o)) begin
            // bus is telling us something
            if (we_i) begin
                // bus is writing to us
                max_prime <= dat_i;
                state <= RESET_ALL_TO_PRIME;
                busy <= 1'b1;
                set_addr<=dat_i[PRIME_BITS-1:1]; // effectively dat_i>>1
                set_val<=1'b1;
                set_enable<=1'b1;
            end else begin
                // bus is reading from us
                // nothing really to do
            end
            ack<=1'b1; // tell master we heard the request
        end else begin
            ack<=1'b0;
            case (state)
                // various delayed states..

                /*
                Clear everything back to 1 (prime) in storage. Start from last bit and work down to 0th ack
                On 0th bit the value should be 0 (1 and zero are by definition not prime even though there could be
                an argument to the contrary)
                Then on next clock cycle we should be in RUN_SIEVE state
                */
                RESET_ALL_TO_PRIME: begin
                    if (set_addr==1) begin
                        // on the next rising edge addr will be 0, so set val going low here
                        // means 0th bit in array gets set low, which means 0 and 1 will be "not prime". 
                        // Two on the other hand is prime, but we have to special case it as well later because it is even. 
                        set_val<=1'b0;
                    end

                    if (set_addr==0) begin
                        // on next clock we will be running the sieve
                        state <= RUN_SIEVE;
                    end

                    else begin
                        // default case, keep counting down, setting bits to 1
                        set_addr<=set_addr-1;
                    end
                end

                /*
                Outer loop of the sieve. Grrab the next prime number, and set the write address pointer to prime square, 
                but right-shifted, since we are storing only odd number.
                */
                RUN_SIEVE: begin
                    if (!next_prime_stall) begin // if next prime not yet stabilized, skip cycle and wait for it
                        if (done) begin // done would mean the next prime square is greater than max prime, so we can exit sieve
                            set_enable<=1'b0; // done writing to storage
                            state <= IDLE; // FSM state IDLE
                            busy <= 1'b0; // turn off the stall signall. Also means read access to stoarge will now come from outside this circuit
                        end
                        else begin // usual condition, we aren't done yet, new prime, and run with it
                            state <= RUN_SIEVE_LOOP; // next clock cycle we will be in the inner loop
                            prime<=next_prime; // grab that next prime
                            set_addr<=(next_prime_sqr[PRIME_BITS-1:1]); // effectively next_prime_sqr>>1, start from (next_prime**2) /2
                            request_next_prime<=1; // ask the other circuit to start looking for yet another prime, while we go on to process on this one
                            multiple_number <=next_prime; // not needed, but handy for debugging in simulation. at any point in the inner loop this will be the n such that (n*prime) is NOT prime
                        end
                    end
                end

                /*
                The inner loop of the sieve. We have a prime number. Loop through multiples of that prime and mark them as NOT prime.
                Note that the outer loop initialized the multiplicand to prime as well, meaning we are starting at prime*prime. 
                We started with prime 3, so all the primes we will see here are ODD, and ODD*ODD = ODD. We only want to staore ODD primes,
                so we can skip to the next ODD prime by stepping up 2*prime each time.
                For example, with prime=3, we start at 3*3=9, then do 3*5, 3*7, 3*9, etc.. because 3*4 would be even and we don't store those. 
                */
                RUN_SIEVE_LOOP: begin
                    request_next_prime <=0; // only keep flag raised one clock tick so the prime finder doesn't keep stepping to the next one
                    if (set_addr+prime <= {1'b0, max_prime[PRIME_BITS-1:1]} ) begin // is the next multiple past the max?
                        set_addr<= set_addr + prime[PRIME_BITS-2:0]; // count up by the prime number, but since address is dropping last bit, this is equivalent to incrementing by 2*prime
                        multiple_number <= multiple_number+2;
                    end
                    else
                        state<=RUN_SIEVE; // start on next prime loop
                end

                /*
                Should never get here, but just in case, let's get it back to a known state.
                Also, this makes the warnings about case not being complete go away
                */
                default: begin
                    state <= IDLE;
                    busy <= 1'b0;
                end
            endcase
        end
    end

    // Link up the registers inside this module to output signals to the outside world. 
    assign stall_o = busy;
    assign ack_o = ack;
    
    // dat_o is the "isPrime" output. If the input (address_i) was odd, we use the storage to look it up, if even, we have to special case the
    // number 2 as being prime. 
    assign dat_o = (stored_is_prime & address_i[0]) | address_i==2;
endmodule


module find_next_prime
#(parameter PRIME_BITS=20)
(
    input clk_i,
    input rst_i,
    input wire request_next_prime_i,
    input wire stored_is_prime_i, // is the number presented in next_prime or address_i prime as far as we know, register is inside prime_storage, this is a tap so we can see the value
    input [PRIME_BITS-1:0] max_prime_i,    
    output [PRIME_BITS-1:0] next_prime_o,
    output [PRIME_BITS-1:0] next_prime_sqr_o,
    output stall_o, // when high, we haven't picked a prime yet, calling module should wait
    output done_o // when high, the next prime sqared is greater than max prime
);

    // FSM states for this module
    localparam NEXT_LOOKUP = 3'b001,
    NEXT_WAIT = 3'b010,
    NEXT_CHECK_DONE = 3'b100;
    
    // local variables
    reg [PRIME_BITS-1:0] next_prime;
    reg [PRIME_BITS-1:0] next_prime_sqr;
    reg [2:0]next_state;
    reg done;
    
    // wire local variables to output wires 
    assign next_prime_o = next_prime;
    assign next_prime_sqr_o = next_prime_sqr;
    assign stall_o = next_state != NEXT_LOOKUP;
    assign done_o = done;

    // private local variable
    // store and hold this flag, until we actually deal with it. In some FSM states, we might not be able to, but
    // calling module is only going to hold the request_next_prime_i high for one clock tick.  
    reg next_prime_requested;
    
    always @ (posedge clk_i)
    if (rst_i) begin
        next_state <= NEXT_LOOKUP;
        next_prime <= 3;
        next_prime_sqr <= 3*3;
        done<=0;
        next_prime_requested<=0;
    end
    else begin
        case (next_state)
            /*
            check that the number we are going to present as the next prime actually still apears to be, based on the storate array.
            If not, skip to next possible prime, and check that one
            */
            NEXT_LOOKUP: if (!stored_is_prime_i || next_prime_requested) begin
                next_prime <= next_prime+2;
                next_state <=NEXT_WAIT;
                next_prime_requested<=0;
            end
            else
                next_prime_requested<= next_prime_requested | request_next_prime_i;
                
            /*
            next prime has been bumped, recalculate the square, will be available on next clock
            */
            NEXT_WAIT: begin
                next_prime_sqr <= next_prime * next_prime;
                next_state<=NEXT_CHECK_DONE;
                next_prime_requested<= next_prime_requested | request_next_prime_i;
            end
            
            /*
            next_prime has been updated and we now have the square of it available. Check if it is past the max prime,
            which would mean we are done.
            */
            NEXT_CHECK_DONE: begin
                if (next_prime_sqr>max_prime_i)
                    done <= 1;
                next_state <= NEXT_LOOKUP;
                next_prime_requested<= next_prime_requested | request_next_prime_i;
            end
            
            default: 
                next_state <= NEXT_LOOKUP;
        endcase
    end

endmodule

/*
storage is a local module, used by the prime_number_core which is above in the file.
This just stores a big bit packed array. The size is passed in from prime_number_core when instantiating.
The main reason for breaking this out, instead of just inlining a big array in the main module is to
prevent vivado from getting confused and trying to synthesize the whole array as individual bits, but
instead to use block ram. So storage module provides a very clear interface for vivado to "infer" that it 
needs RAM.
*/
module storage 
#(parameter STORAGE_BITS=19)
(
    input clk_i,
    input [STORAGE_BITS-1:0] r_address_i,
    output r_bit,
    input [STORAGE_BITS-1:0] w_address_i,
    input w_bit,
    input we_i
);
    (* ram_style = "block" *) // pragma, coax Vivado to give us block ram even if it thinks it has a better option
    reg bit_data [0:(2**STORAGE_BITS)-1]; // ODD number prime'ness stored here 
    reg data_bit; // delay store-and-forward of the read value, needed to help Vivado infer bit_data is block ram
    
    // link output of data_bit to r_bit output bit_data
    // this store-and-forward through data_bit means the signal is one clock tick behind, but that
    // is needed, else Vivado refuses to infer bit_data as block ram, and tries to implement it as bits in the fabric
    assign r_bit = data_bit;
    
    // at each clock cycle, if the write enable signal is on, set the bit at w_address_i to the value w_bit
    always @ (posedge clk_i)
    if (we_i)
        bit_data[w_address_i] = w_bit;

    // At each clock cycle read a bit and send it out on data_bit, which will get linked to r_bit, and relayed out of the module
    always @ (posedge clk_i)
    data_bit = bit_data[r_address_i];
endmodule

