(*
	Greyson Potter - 04/06/2020 - https://github.com/gkpotter

	PrimeOCamlFunctional.ml : A functional implementation of Dave Plummer's prime 
	sieving algorithm in OCaml.
*)

open Core

type prime_sieve = {
	sieve_size : int; 
	raw_bits : bool array
}

let  prime_counts = Map.of_alist_exn (module Int)
	[ (10,4);
		(100, 25);              
		(1000, 168);
		(10000, 1229);
		(100000, 9592);
		(1000000, 78498);
		(10000000, 664579);
		(100000000, 5761455)
 	]
;;

let run_sieve sieve = 
	let q = Int.of_float (Float.sqrt (Float.of_int sieve.sieve_size)) in 
	let rec _run_sieve factor num =
		if num < sieve.sieve_size then (
			Array.set sieve.raw_bits (num/2) false;
			_run_sieve factor (num+factor*2)
		)
		else
			let rec get_next_factor num =
				if Array.get sieve.raw_bits (num/2) then
					num
				else
					get_next_factor (num+2)
			in
			let factor = get_next_factor (factor+2) in
			if factor < q then
				_run_sieve factor (3*factor)
		in
		_run_sieve 3 9
;;

let count_primes sieve = 
	Array.fold sieve.raw_bits ~init:0 ~f:(fun total bit -> 
		if bit then total + 1
		else total)
;;

let validate_results sieve =
	match Map.find prime_counts sieve.sieve_size with
	| Some num -> num = (count_primes sieve)
	| None -> false
;;

let print_results sieve show_results duration passes =
	if show_results then printf "2, ";
	let count = List.fold (List.range ~stride:2 3 sieve.sieve_size) 
		~init:1 
		~f:(fun total num -> 
			if Array.get sieve.raw_bits (num/2) 
			then (
				if show_results then printf "%d, " num;
				total + 1
			)
			else total)
	in
	if show_results then printf "... \nCount: %d\n" count;
	printf "gkpotter-unfaithful;%d;%f;1;algorithm=base,faithful=no\n" 
		passes
		duration
;;

let create_sieve limit = 
	{sieve_size = limit; raw_bits = (Array.init ((limit+1)/2) ~f:(fun _ -> true))}
;;

let () =
	let start_time = Unix.gettimeofday () in
	let limit = 1000000 in
	let rec run_test sieve passes =
		if Float.(>) ((Unix.gettimeofday ()) -. start_time) 5.0 then
			let duration = ((Unix.gettimeofday ()) -. start_time) in
			print_results sieve false duration passes 
		else
			let sieve = (create_sieve limit) in
			run_sieve sieve;
			run_test sieve (passes+1)
	in
	run_test (create_sieve limit) 0
;;