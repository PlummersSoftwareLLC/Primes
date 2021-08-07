(ns primes
  "Clojure implementation of Sieve of Eratosthenes by Alex Vear (axvr)

  This implementation is fast and faithful to Dave's original implementation."
  (:import [java.time Instant Duration]))


;; Disable overflow checks on mathematical ops and warn when compiler is unable
;; to optimise correctly.
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn sieve [limit]
  (let [limit (int limit)
        sieve (boolean-array limit true)
        q     (int (inc (Math/sqrt limit)))]
    ;; Highly optimised Sieve of Eratosthenes algorithm.
    (loop [factor 3]
      (when (< factor q)
        (if (aget sieve factor)
          (let [factor*2 (+ factor factor)]
            (loop [num (* factor factor)]
              (when (<= num limit)
                (aset sieve num false)
                (recur (+ num factor*2))))))
        (recur (+ 2 factor))))
    ;; Return sequence of found prime numbers.
    (cons 2 (->> (range 3 limit 2)
                 (map #(when (aget sieve %) %))
                 (filter some?)))))


(def prev-results
  "Previous results to check against sieve results."
  {10          4
   100         25
   1000        168
   10000       1229
   100000      9592
   1000000     78498
   10000000    664579
   100000000   5761455
   1000000000  50847534
   10000000000 455052511})


(defn benchmark
  "Benchmark Sieve of Eratosthenes algorithm."
  []
  (let [limit       1000000
        start-time  (Instant/now)
        start-milli (.toEpochMilli start-time)]
    (loop [pass 1]
      (let [primes   (sieve limit)
            cur-time (System/currentTimeMillis)]
        (if (< (- cur-time start-milli) 5000)
          (recur (inc pass))
          ;; Return benchmark report.
          {:primes primes
           :passes pass
           :limit  limit
           :time   (Duration/between start-time (Instant/now))
           :valid? (= (count primes)
                      (prev-results limit))})))))


;; Reenable overflow checks on mathematical ops and turn off warnings.
(set! *warn-on-reflection* false)
(set! *unchecked-math* false)


(defn format-results
  "Format benchmark results into expected output."
  [{:keys [primes passes limit time valid?]}]
  (let [nanos (.toString (.toNanos time))
        timef (str (subs nanos 0 1) "." (subs nanos 1))]
    (str "Passes: " passes ", "
         "Time: " timef ", "
         "Avg: " (float (/ (/ (.toNanos time) 1000000000) passes)) ", "
         "Limit: " limit ", "
         "Count: " (count primes) ", "
         "Valid: " (if valid? "True" "False")
         "\n"
         "axvr;" passes ";" timef ";1;algorithm=base,faithful=yes,bits=8")))


(defn run [_]
  (println (format-results (benchmark))))
