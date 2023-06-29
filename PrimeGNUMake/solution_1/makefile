GMSL_NO_WARNINGS := true
include gmsl/gmsl

two := x x
three := x x x

ifndef sieve_size
	sieve_size := 1000
endif

ifndef time_to_run
	time_to_run := 5
endif

set_up = 	$(or \
				$(eval factor := $(three)),\
				$(eval sieve_size_encode := $(call int_encode,$1)),\
				$(eval rawbits := $(call int_halve,$(sieve_size_encode))),\
				$(eval odd_number := x),\
				$(eval sub := $(sieve_size_encode)),\
		  	)

bit_is_true = $(filter x,$(word $(call int_decode,$(call int_inc,$(call int_halve,$1))),$(rawbits)))

find_factor =	$(if $(call bit_is_true,$(num)),\
					$(eval factor := $(num)),\
					$(eval num += $(two))\
					$(if $(call int_gt,$(num),$(sieve_size_encode)),,\
						$(call find_factor)\
					)\
				)

clear_bits =	$(if $(call int_gt,$(num),$(sieve_size_encode)),,\
					$(eval half := $(call int_halve,$(num)))\
					$(eval rawbits := $(wordlist 1,$(call int_decode,$(half)),$(rawbits)) f $(wordlist $(call int_decode,$(call int_plus,$(half),$(two))),$(call int_decode,$(rawbits)),$(rawbits)))\
					$(eval num := $(call int_plus,$(num),$(call int_multiply,$(factor),$(two))))\
					$(call clear_bits)\
			  	)

run_sieve = $(if $(call int_eq,$(sub),),,\
				$(eval sub := $(call int_subtract,$(sub),$(odd_number)))\
				$(eval odd_number += $(two))\
				$(eval num := $(factor))\
				$(call find_factor)\
				$(eval num := $(call int_multiply,$(factor),$(three)))\
				$(call clear_bits)\
				$(eval factor += $(two))\
				$(call run_sieve)\
			)

print_num := $(three)
results := 2
run_print_results = $(foreach a,$(rawbits),\
					$(or \
						$(and \
							$(if $(call bit_is_true,$(print_num)),\
								$(eval results := $(results), $(call int_decode,$(print_num)))\
							),\
						),\
						$(eval print_num += $(two)),\
					)\
				)

count_primes =  $(and \
					$(foreach a,$(rawbits),\
						$(if $(filter x,$(a)),\
							$(eval total_primes := $(call int_inc,$(total_primes)))\
						),\
					),\
				)

run_sieve_loop =	$(if $(filter 1,$(shell echo $$(( $$(date +%s) - $(start_time) > $(time_to_run) )))),,\
						$(call set_up,$(sieve_size))\
						$(call run_sieve)\
						$(eval iterations := $(call int_inc,$(iterations)))\
						$(call run_sieve_loop)\
					)
				 
all:;
	@echo   $(eval start_time := $(shell echo $$(date +%s)))\
			$(call run_sieve_loop)\
			$(eval final_time := $(shell echo $$(( $$(date +%s) - $(start_time)))))\
			$(eval iterations_decoded := $(call int_decode,$(iterations)))\
			$(call count_primes)\
			$(if $(filter true,$(print_results)),\
				$(call run_print_results)\
				$(results),)
	@echo	Passes: $(iterations_decoded), Time: $(final_time), Limit: $(sieve_size), Count: $(call int_decode,$(total_primes))
	@echo	jastein693\;$(iterations_decoded)\;$(final_time)\;1\;algorithm=base,faithful=no
