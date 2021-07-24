(*
	Greyson Potter - 04/18/2021 - https://github.com/gkpotter
	
	PrimeOCamlFunctional.ml : A functional implementation of Dave Plummer's prime 
	sieving algorithm in OCaml.
*)

open Core

class prime_sieve limit = 
	object (self)
		val sieve_size = limit
		val raw_bits = Array.init ((limit+1)/2) ~f:(fun _ -> true)
		val prime_counts = (Map.of_alist_exn (module Int)
			[ (10, 4);
				(100, 25);              
				(1000, 168);
				(10000, 1229);
				(100000, 9592);
				(1000000, 78498);
				(10000000, 664579);
				(100000000, 5761455)
		 	]
		)

		method validate_results =
			match Map.find prime_counts sieve_size with
			| Some num -> num = self#count_primes
			| None -> false

		method run_sieve = 
			let q = Int.of_float (Float.sqrt (Float.of_int sieve_size)) in 
			let factor = Ref.create 3 in
			while !factor < q do
				let next_factor_found = Ref.create false in
				let num = Ref.create !factor in
				while not !next_factor_found do
					if Array.get raw_bits (!num/2) then (
						factor := !num;
						next_factor_found := true
					)
					else
						num := !num + 2
				done;
				num := !factor * 3;
				while !num < sieve_size  do
					Array.set raw_bits (!num/2) false;
					num := !num + !factor * 2
				done;
				factor := !factor + 2
			done;

		method count_primes = 
			Array.fold raw_bits ~init:0 ~f:(fun total bit -> 
				if bit then total + 1
				else total)

		method print_results show_results duration passes =
			if show_results then printf "2, ";
			let count = List.fold (List.range ~stride:2 3 sieve_size) 
				~init:1 
				~f:(fun total num -> 
					if Array.get raw_bits (num/2) 
					then (
						if show_results then printf "%d, " num;
						total + 1
					)
					else total)
			in
			if show_results then printf "... \nCount: %d\n" count;
			printf "gkpotter-faithful;%d;%f;1;algorithm=base,faithful=yes\n" 
				passes
				duration
	end
;;

let () =
	let start_time = Unix.gettimeofday () in
	let limit = 1000000 in
	let sieve = Ref.create (new prime_sieve limit) in
	let passes = Ref.create 0 in
	while Float.(<) ((Unix.gettimeofday ()) -. start_time) 5.0 do
		sieve := new prime_sieve limit;
		!sieve#run_sieve;
		passes := !passes + 1
	done;
	let duration = ((Unix.gettimeofday ()) -. start_time) in
	!sieve#print_results false duration !passes; 
;;