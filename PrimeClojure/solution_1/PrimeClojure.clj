(import java.util.BitSet)

(defprotocol PrimeClojureProtocol
    (resultsDictionary [this])
    (validateResults [this])
    (getBit [this index])
    (clearBit [this index])
    (runSieve [this])
    (printResults [this showResults duration passes])
    (countPrimes [this]))

(deftype PrimeClojure [sieveSize bits]
    PrimeClojureProtocol

    (resultsDictionary [this] 
        { 10 4
          100 25
          1000 168
          10000 1229
          100000 9592
          1000000 78498
          10000000 664579
          100000000 5761455
          1000000000 50847534
          10000000000 455052511 })

    (validateResults [this] 
        (== (get (. this resultsDictionary) sieveSize) 
            (. this countPrimes)))

    (getBit [this index] 
        (.get bits (bit-shift-right index 1)))

    (clearBit [this index] 
        (.clear bits (bit-shift-right index 1)))

    (runSieve [this] 
        (def factor 3)
        (def q (Math/floor (Math/sqrt sieveSize)))
        
        (while (<= factor q)
            (loop [num factor]
                (if (. this getBit num) 
                    (def factor num)
                    (if (< num sieveSize) 
                        (recur (+ num 2)))))

            (loop [num (* factor factor)]
                (. this clearBit num)
                (if (< num sieveSize) 
                    (recur (+ num (* factor 2)))))
            
            (def factor (+ factor 2))))

    (printResults [this showResults duration passes]
        (if showResults 
            (printf "2, "))
        
        (def total (if (>= sieveSize 2) 1 0))

        (loop [num 3]
            (if (. this getBit num) 
                (do (if showResults 
                        (printf "%d, " num))
                    (def total (inc total))))
            (if (<= num sieveSize) 
                (recur (+ num 2))))

        (if showResults 
            (do (printf "\n")
                (printf "Passes: %d, Time: %f, " passes duration)
                (printf "Avg: %f, Limit: %d, " (/ duration passes) sieveSize)
                (printf "Count1: %d, Count2: %d, " total (. this countPrimes))
                (printf "Valid: %s\n" (if (. this validateResults) "True" "False"))
                (printf "\n")))

        (printf "mmcdon20_clojure;%d;%f;1;algorithm=base,faithful=yes,bits=1\n"
            passes duration))

    (countPrimes [this] 
        (.cardinality bits)))

(defn primeSieve [sieveSize]
    (def bits (BitSet. (bit-shift-right (+ sieveSize 1) 1)))
    (.flip bits 0 (bit-shift-right (+ sieveSize 1) 1))
    (PrimeClojure. sieveSize bits))

(def passes 0)
(def start (System/currentTimeMillis))

(loop []
    (def sieve (primeSieve 1000000))
    (.runSieve sieve)
    (def passes (inc passes))
    (def stop (System/currentTimeMillis))
    (if (>= (- stop start) 5000) 
        (.printResults sieve false (/ (- stop start) 1000.0) passes) 
        (recur)))
