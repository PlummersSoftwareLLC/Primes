#!/usr/bin/cmake -P

set(PRIMES_COUNT_REFERENCE
             10 4            # Historical data for validating our results - the number of primes
            100 25           # to be found under some limit, such as 168 primes under 1000
           1000 168
          10000 1229
         100000 9592
        1000000 78498
       10000000 664579
      100000000 5761455
     1000000000 50847534
    10000000000 455052511
)

function(primes_get_count_reference result size)
    list(LENGTH PRIMES_COUNT_REFERENCE nbitems)
    set(i 0)
    while(i LESS nbitems)
        list(GET PRIMES_COUNT_REFERENCE ${i} size_ref)
        if(size_ref EQUAL size)
            math(EXPR count_index "${i}+1")
            list(GET PRIMES_COUNT_REFERENCE ${count_index} count_ref)
            set(${result} ${count_ref} PARENT_SCOPE)
            return()
        endif()
        math(EXPR i "${i}+2")
    endwhile()
    set(${result} "NOTFOUND" PARENT_SCOPE)
endfunction()


function(sqrt out value)
    set(x ${value})
    while(1)
        # Do 1 Newton-Rhapson step
        math(EXPR dx "(${x}*${x}-${value})/(2*${x})")
        math(EXPR xn "${x}-${dx}")
        set(x ${xn})
        if(dx LESS_EQUAL 1)
            set(${out} ${xn} PARENT_SCOPE)
            return()
        endif()
    endwhile()
endfunction()

function(microseconds_to_seconds result useconds)
    set(t "0000000${useconds}")
    string(LENGTH ${t} tlength)
    math(EXPR s_length "${tlength}-6")
    string(SUBSTRING ${t} 0 ${s_length} t_s)
    string(SUBSTRING ${t} ${s_length} -1 t_us)
    math(EXPR t_s "${t_s}")  # Remove leading zeros
    set(${result} "${t_s}.${t_us}" PARENT_SCOPE)
endfunction()

function(is_prime out sieve nb)
    math(EXPR idx "${nb}*")

endfunction()

function(run_sieve result size)
    # Mark all uneven numbers >=3 as prime
    set(i 3)
    while(i LESS_EQUAL size)
        set(sieve_${i} 1)
        math(EXPR i "${i}+2")
    endwhile()

    set(factor 3)
    sqrt(q ${size})
    while(factor LESS_EQUAL q)

        # Find next prime that is >=factor
        set(num ${factor})
        while(num LESS size)
            if(sieve_${num})
                set(factor ${num})
                break()
            endif()
            math(EXPR num "${num}+2")
        endwhile()

        # Mark factor^2 + i * 2 * factor (i >= 0) as prime
        math(EXPR num ${factor}*${factor})
        math(EXPR step "2*${factor}")
        while(num LESS_EQUAL size)
            set(sieve_${num} 0)
            math(EXPR num ${num}+${step})
        endwhile()
        math(EXPR factor ${factor}+2)
    endwhile()

    # Count all primes from >= 3, assume 2 is a prime
    set(count 1)
    set(num 3)
    while(num LESS_EQUAL size)
        if(sieve_${num})
            math(EXPR count "${count}+1")
        endif()
        math(EXPR num "${num}+2")
    endwhile()
    set(${result} ${count} PARENT_SCOPE)
endfunction()

function(primes_drag_race size seconds)
    primes_get_count_reference(reference_count ${size})

    string(TIMESTAMP t_start "%s%f")
    set(t_current ${t_start})
    math(EXPR t_deadline "${t_start}+${seconds}*1000000")
    set(passes 0)
    while(t_current LESS t_deadline)
        run_sieve(count ${size})
        if(reference_count AND NOT reference_count EQUAL count)
            message(FATAL_ERROR "Invalid count for sieve size=${size} expected count=${reference_count} actual count=${count}")
        endif()
        math(EXPR passes "${passes}+1")
        string(TIMESTAMP t_current "%s%f")
    endwhile()
    string(TIMESTAMP t_finish "%s%f")

    math(EXPR dt_us "${t_finish}-${t_start}")
    microseconds_to_seconds(duration ${dt_us})

    math(EXPR average_duration_us "${dt_us}/${passes}")
    microseconds_to_seconds(average_duration ${average_duration_us})

    if(reference_count)
        set(valid_str "true")
    else()
        set(valid_str "false")
    endif()

    message("Passes: ${passes}, Time: ${duration}, Avg: ${average_duration} (sec/pass), Limit: ${size}, Count: ${count}, Valid: ${valid_str}")
    message("")
    message("madebr_cmake;${passes};${duration};1;algorithm=base,faithful=no")
endfunction()

if(NOT DEFINED SIEVE_SIZE)
    set(SIEVE_SIZE 1000000)
endif()

if(NOT DEFINED DURATION)
    set(DURATION 5)
endif()

primes_drag_race(${SIEVE_SIZE} ${DURATION})
