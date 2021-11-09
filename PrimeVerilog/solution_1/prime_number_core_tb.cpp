#include "Vprime_number_core.h"
#include <time.h>
#include <stdio.h>

  Vprime_number_core* top = NULL;

  void doSieve() {

    // destroy previous simulation instance
    if (top!=NULL) {
        top->final();
        delete top;
    }

    // brand new simulation
    top = new Vprime_number_core;

    // clock starts low
    top->clk_i = 0;
    top->eval();

    // first "up" tick of clock we give it a reset
    top->clk_i = 1;
    top->rst_i = 1;
    top-> eval(); 
    top->clk_i = 0;
    top->rst_i = 0;
    top-> eval(); 

    // ask our prime core to run the sieve
    // "up" tick, give it our max prime (1000000)
    top->clk_i = 1;
    top->we_i = 1;
    top->dat_i = 1000000;
    top->stb_i =1;
    top-> eval(); 
    // "down" tick, clear write enable and storbe
    top->clk_i=0;
    top->we_i = 0;
    top->stb_i =0;
    top->eval();

    // wait for the prime core to finish
    while (top->stall_o) { 
        top->clk_i = 1;
        top-> eval(); 
        top->clk_i = 0;
        top-> eval(); 
    }

  }

  int main(int argc, char **argv, char **env) {
    Verilated::commandArgs(argc, argv);
    clock_t start_time = clock();
    clock_t pencil_down_time = start_time + 5*CLOCKS_PER_SEC;

    clock_t current_time = start_time;

    unsigned loopCount =0;
    do {
        doSieve();
 
        loopCount++;
        current_time = clock();
    } while (current_time< pencil_down_time); 
    clock_t stop_time = clock();

    unsigned primeCount = 0;
    for (unsigned long k=1; k<= 1000000; k++) {
        top->address_i = k;
        top->clk_i = 1;
        top-> eval(); 
        top->clk_i = 0;
        top-> eval(); 
        primeCount += top->dat_o ? 1:0;
    }
    //printf("primes: %d \n", primeCount);
    //printf("loops: %d \n", loopCount);

    float elapsed_time = ((float)(stop_time-start_time)) / CLOCKS_PER_SEC;
    //printf("elapsed: %f \n", elapsed_time);

    printf("%s;%d;%0.1f;1;algorithm=base,faithful=yes,bits=1\n", "alwayslinux2-verilog", loopCount, elapsed_time);


    top->final();
    delete top;
    exit(0);
  }