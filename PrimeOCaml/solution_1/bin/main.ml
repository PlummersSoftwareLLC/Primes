open Core
open PrimeOCaml

let () =
	let  now_in_seconds_as_float ()  =  (Int63.to_float (Time_ns.to_int63_ns_since_epoch (Time_ns.now ()))) /. 1000000000.0 in
	let start_time = now_in_seconds_as_float () in
	let limit = 1000000 in
	let sieve = Ref.create (new prime_sieve limit) in
	let passes = Ref.create 0 in
	while Float.(<) ((now_in_seconds_as_float ()) -. start_time)  5.0 do
		sieve := new prime_sieve limit;
		!sieve#run_sieve;
		passes := !passes + 1
	done;
	let duration = (now_in_seconds_as_float ()) -. start_time in
	!sieve#print_results false  duration !passes; 
;;