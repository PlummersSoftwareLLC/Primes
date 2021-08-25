datatype 'a stream = Empty | Cons of 'a * (unit -> 'a stream)

fun count start = Cons (start, fn () => count (start+1))

fun map _ Empty = Empty
  | map f (Cons (h, t)) = Cons (f h, fn () => map f (t ()))

fun until _ Empty = Empty
  | until f (Cons (h, t)) = 
    if f h then
        Empty
    else
        Cons (h, fn () => until f (t ()))

fun filter _ Empty = Empty
  | filter f (Cons (h, t)) =
    if f h then
        Cons (h, fn () => filter f (t()))
    else
        filter f (t ())

fun find _ Empty = NONE
  | find f (Cons (h, t)) =
    if f h then
        SOME h
    else
        find f (t ())

fun app _ Empty = ()
  | app f (Cons (h, t)) =
    let val _ = f h in app f (t ()) end

fun create_sieve limit =
    let val bit_array_size = (limit + 1) div 2
        val bit_array =  BitArray.bits (bit_array_size, [])
        fun get_bit index = not (BitArray.sub (bit_array, (index div 2)))
        fun clear_bit index = BitArray.setBit (bit_array, (index div 2))
        fun count_primes () = bit_array_size - (BitArray.foldl (fn (x,y) => if x then 1 + y else y) 0 bit_array)
        fun get_found_primes () = Cons(2, fn () => filter get_bit (until (fn x => x >= limit) (map (fn x => x*2+3) (count 0))))
        fun run_sieve () = 
            let val starting_factor = 3
                val q = Real.floor (Math.sqrt (real limit))
                fun run factor =
                    if factor <= q then
                        let 
                            (* Find next prime *)
                            val numbers = until (fn x => x > q) (map (fn x => x * 2 + factor) (count 0))
                            val number = find get_bit numbers
                            val new_factor = case number of
                                              SOME value => value
                                            | NONE => factor
                            (* Clear prime's multiples *)
                            val clear_start = new_factor * new_factor
                            val increment = new_factor * 2
                            val clear_indices = Vector.tabulate ((limit - clear_start) div increment + 1, fn x => x*increment+clear_start)
                            val _ = Vector.app clear_bit clear_indices
                        in
                            run (new_factor + 2)
                        end
                    else
                        ()
            in
                run starting_factor
            end
        fun validate_results () = 
            let 
                val prime_counts = (10, 4)::
                                   (100, 25)::
                                   (1000, 168)::
                                   (10000, 1229)::
                                   (100000, 9592)::
                                   (1000000, 78498)::
                                   (10000000, 664579)::
                                   (100000000, 5761455)::[]
                fun find_value [] _ = NONE
                  | find_value ((label, count)::rest) value =
                    if label = value then
                        SOME count
                    else
                        find_value rest value
                val optional_count = SOME (count_primes ())
                val valid_prime_count = find_value prime_counts limit 
            in
                optional_count = valid_prime_count    
            end
        fun print_results show_results duration passes =
            let 
                val () = if show_results 
                then 
                    app (fn x => print ((Int.toString x) ^ " ")) (get_found_primes ())
                else 
                    ()
            in
                print ("NotMatthewGriffin_SML;" ^ (Int.toString passes) ^ ";" ^ (Time.toString duration) ^ ";1;algorithm=base,faithful=yes,bits=1")
            end
    in
        {
            sieve_size = limit,
            count_primes = count_primes,
            run_sieve = run_sieve,
            validate_results = validate_results,
            print_results = print_results,
            get_found_primes = get_found_primes
        }
    end


(* take a function f and count how many times it executes in seconds *)
fun count_executions f seconds =
    let val start = Time.now ()
        val allowed_time = Time.fromSeconds seconds
        fun loop count last_result  =
            let val compare_time = Time.now ()
                val elapsed_time = Time.- (compare_time, start)
            in
                if Time.>= (elapsed_time, allowed_time) then
                    (count, elapsed_time, last_result)
                else
                    let val f_result = f () in
                        loop (count + 1) (SOME f_result)
                    end
            end
    in
        loop 0 NONE
    end


fun main (arg0: string,  args: string list) =  
    let  
        fun benchmark () =
            let 
                val sieve = create_sieve 1000000 
                val () = (# run_sieve sieve) ()
            in
                sieve
            end
        val (passes, duration, SOME sieve) = count_executions benchmark 5 
        val () = (# print_results sieve) false duration passes
    in
        OS.Process.success
    end

