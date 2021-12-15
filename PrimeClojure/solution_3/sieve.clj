(ns sieve
  "Clojure implementation of Sieve of Eratosthenes by Peter Strömberg (a.k.a. PEZ)

  This implementation is faithful to Dave's original implementation,
   see `README.md` for a bit more on this (or the code below, of course)"
  (:require [criterium.core :refer [with-progress-reporting bench quick-bench]])
  (:import [java.time Instant Duration]))


;; Disable overflow checks on mathematical ops and warn when compiler is unable
;; to optimise correctly.
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defn sieve [^long n]
  (let [primes (boolean-array (inc n) true)
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (if (< n 2)
      '()
      (loop [p 3]
        (if (< sqrt-n p)
          (let [num-slices 8 ; I'm not sure what is a good default
                num-slices (if (and (zero? ^long (mod n num-slices))
                                    (> n 1000))
                             num-slices
                             1)
                slice-size (quot n num-slices)
                futures (mapv (fn [^long slice-num]
                                (future
                                  (let [start (inc (* slice-num slice-size))
                                        end (dec (+ start slice-size))
                                        start (if (= start 1) 3 start)]
                                    (loop [res (transient [])
                                           i start]
                                      (if (<= i end)
                                        (recur (if (aget primes i)
                                                 (conj! res i)
                                                 res)
                                               (+ i 2))
                                        (persistent! res))))))
                              (range num-slices))]
            (into [2]
                  (mapcat deref)
                  futures))
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (<= i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur  (+ p 2))))))))

(comment
  (sieve 1)
  ;; => ()

  (sieve 10)
  ;; => [2 3 5 7]

  (sieve 100)
  ;; You try it!

  ;; `doall` is not strictly necessary for this sieve, because it is not lazy,
  ;; but for good measure =)
  (with-progress-reporting (quick-bench (doall (sieve 1000000))))
  (quick-bench (doall (sieve 1000000)))

  ;; This one takes a lot of time, you have been warned
  (with-progress-reporting (bench (doall (sieve 1000000))))
  )

(def prev-results
  "Previous results to check against sieve results."
  {1           0
   10          4
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
        end-by      (+ (.toEpochMilli start-time) 5000)]
    (loop [pass 1]
      (let [primes   (sieve limit)
            n        (count primes)
            cur-time (System/currentTimeMillis)]
        (if (<= cur-time end-by)
          (recur (inc pass))
          ;; Return benchmark report.
          {:primes primes
           :passes pass
           :limit  limit
           :time   (Duration/between start-time (Instant/now))
           :valid? (= n
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
         "pez-clj;" passes ";" timef ";8;algorithm=base,faithful=yes,bits=8")))


(defn run [{:keys [warm-up?]
            :or   {warm-up? false}}]
  (when warm-up?
    ;; Warm-up reduces the variability of results.
    (format-results (benchmark)))
  (println (format-results (benchmark))))