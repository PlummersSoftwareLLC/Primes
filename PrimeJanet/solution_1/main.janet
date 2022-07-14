(def limit 1000000)

(defn sieve [n]
  (let [primes (array/new-filled n true)
        sqrt-n (math/ceil (math/sqrt n))]
    # remove all even numbers
    (loop [j :range [0 n 2]] (put primes j false))
    # fix 1 and 2 manually
    (put primes 1 false)
    (put primes 2 true)
    # 3 will be the first candidate we test
    (var p 3)
    (if (< n 2)
      @[] 
      (do (while (<= p sqrt-n)
            (when (get primes p)
              (var i (* p p))
                (while (<= i n)
                  (put primes i false)
                  (set i (+ i p p))))
            (set p (+ p 2)))
        # return the primes array as numbers
          primes))))

(defn benchmark
  "Runs the sieve for 5s and prints the results to std out"
  [sieve-fn limit]
  (var pass 1)
  (let [start-time  (os/clock)
        end-by      (+ start-time 5.0)
        _           (while (<= (os/time) end-by)
                      (let [primes   (sieve-fn limit)]
                        (set pass (inc pass))))]
    (print (string "Musab-Nazir;" pass ";" (- (os/clock) start-time) ";" 1 
                   ";algorithm=base,faithful=yes"))))

(benchmark sieve limit)
