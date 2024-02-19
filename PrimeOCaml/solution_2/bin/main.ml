open Core
open PrimeOCamlFunctional

let () =
	let  now_in_seconds_as_float ()  =  (Int63.to_float (Time_ns.to_int63_ns_since_epoch (Time_ns.now ()))) /. 1000000000.0 in
	let start_time = now_in_seconds_as_float () in
	let limit = 1000000 in
	let rec run_test sieve passes =
		if Float.(>) ((now_in_seconds_as_float ()) -. start_time) 5.0 then
			let duration = ((now_in_seconds_as_float ()) -. start_time) in
			print_results sieve false duration passes 
		else
			let sieve = (create_sieve limit) in
			run_sieve sieve;
			run_test sieve (passes+1)
	in
	run_test (create_sieve limit) 0
;;