(ns sieve-1-bit
  "Clojure implementation of Sieve of Eratosthenes by Alex Vear (axvr)

  This implementation is fast and faithful to Dave's original implementation,
  when run on Linux it is slower than my 8-bit version (for some reason)."
  (:import [java.time Instant Duration]
           java.util.BitSet))


;; Disable overflow checks on mathematical ops and warn when compiler is unable
;; to optimise correctly.
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)


(defmacro << [n shift]
   `(bit-shift-left ~n ~shift))


(defmacro >> [n shift]
  `(unsigned-bit-shift-right ~n ~shift))


;; NOTE: may not make a difference.
(defmacro sqr [n]
  `(unchecked-multiply-int ~n ~n))


(defn sieve
  "This returns a java.util.BitSet object where the index of each true bit is
  the prime number (+ 1 (* 2 index)).  Despite this being unidiomatic, the
  result of this function can be used by the benchmark without needing to
  convert it into a proper list of primes. [1]

  [1]: <https://github.com/PlummersSoftwareLLC/Primes/discussions/794>"
  [^long limit]
  (let [q (inc (int (Math/sqrt limit)))
        sieve (BitSet. (>> limit 1))]
    (loop [factor (int 3)]
      (when (< factor q)
        (when-not (.get sieve (>> factor 1))
          (let [factor*2 (<< factor 1)]
            (loop [num (sqr factor)]
              (when (< num limit)
                (.set sieve (>> num 1))
                (recur (+ factor*2 num))))))
        (recur (+ 2 factor))))
    (.flip sieve 0 (.length sieve))
    sieve))


(defn sieve->primes
  "Function to convert the sieve output to a usable/printable list of primes."
  [^BitSet sieve]
  (let [out (transient [2])
        size (.length sieve)]
    (loop [idx (int 1)]
      (when (pos? idx)
        (when (.get sieve idx)
          (conj! out (inc (<< idx 1))))
        (recur (.nextSetBit sieve (inc idx)))))
    (persistent! out)))


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
  (let [limit      1000000
        start-time (Instant/now)
        end-by     (+ (.toEpochMilli start-time) 5000)]
    (loop [pass 1]
      (let [^BitSet sieve (sieve limit)
            cur-time      (System/currentTimeMillis)]
        (if (<= cur-time end-by)
          (recur (inc pass))
          ;; Return benchmark report.
          (let [finished-at (Instant/now)]
            {:primes (sieve->primes sieve)  ; Construct a printable version of sieve result.
             :passes pass
             :limit  limit
             :time   (Duration/between start-time finished-at)
             :count  (.cardinality sieve)
             :valid? (= (.cardinality sieve) (prev-results limit))}))))))


;; Reenable overflow checks on mathematical ops and turn off warnings.
(set! *warn-on-reflection* false)
(set! *unchecked-math* false)


(defn format-results
  "Format benchmark results into expected output."
  [{:keys [primes passes valid?] :as result}]
  (let [nanos (.toString (.toNanos (:time result)))
        timef (str (subs nanos 0 1) "." (subs nanos 1))]
    (str "Passes: " passes ", "
         "Time: " timef ", "
         "Avg: " (float (/ (/ (.toNanos (:time result)) 1000000000) passes)) ", "
         "Limit: " (:limit result) ", "
         "Count: " (:count result) ", "
         "Valid: " (if valid? "True" "False")
         "\n"
         "axvr_clj_1-bit;" passes ";" timef ";1;algorithm=base,faithful=yes,bits=1")))


(defn run [& _]
  (println (format-results (benchmark)))
  (flush))
