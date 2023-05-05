(ns sieve
  "Clojure implementations of The Sieve of Eratosthenes by Peter Strömberg (a.k.a. PEZ)"
  (:require [clojure.edn])
  (:import [java.time Instant Duration])
  (:gen-class))

;; Disable overflow checks on mathematical ops and warn when compiler is unable
;; to optimise correctly.
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn sieve-v
  "Using regular Clojure persistent (immutable) vectors"
  [^long n]
  (if (< n 2)
    []
    (let [sqrt-n (long (Math/ceil (Math/sqrt n)))
          half-n (bit-shift-right n 1)]
      (loop [p 3
             primes (vec (repeat (bit-shift-right n 1) true))]
        (if (< p sqrt-n)
          (if (nth primes (bit-shift-right p 1))
            (recur (+ p 2) (loop [i (bit-shift-right (* p p) 1)
                                  primes primes]
                             (if (< i half-n)
                               (recur (+ i p) (assoc primes i false))
                               primes)))
            (recur (+ p 2) primes))
          primes)))))

(defn sieve-v-transient
  "Using a transient vector"
  [^long n]
  (if (< n 2)
    []
    (let [sqrt-n (long (Math/ceil (Math/sqrt n)))
          half-n (bit-shift-right n 1)]
      (loop [p 3
             primes (transient (vec (repeat (bit-shift-right n 1) true)))]
        (if (< p sqrt-n)
          (if (nth primes (bit-shift-right p 1))
            (recur (+ p 2) (loop [i (bit-shift-right (* p p) 1)
                                  primes primes]
                             (if (< i half-n)
                               (recur (+ i p) (assoc! primes i false))
                               primes)))
            (recur (+ p 2) primes))
          (persistent! primes))))))

(comment
  (def sieve sieve-v)
  (def sieve sieve-v-transient)
  (defn loot [raw-sieve]
    (keep-indexed (fn [i v]
                    (when v (if (zero? i)
                              2
                              (inc (* i 2)))))
                  raw-sieve))
  (loot (sieve 1))
  (loot (sieve 10))
  (loot (sieve 100))
  (time (count (loot (sieve 1000000))))'
  (require '[criterium.core :refer [bench quick-bench with-progress-reporting]])
  (with-progress-reporting (quick-bench (sieve 1000000))))

(set! *unchecked-math* true)

(defn sieve-ba
  "Java boolean array storage
   Returns the raw sieve with only odd numbers present."
  [^long n]
  (if (< n 2)
    (boolean-array 0)
    (let [sqrt-n (unchecked-long (Math/ceil (Math/sqrt (double n))))
          half-n (unchecked-int (bit-shift-right n 1))
          primes (boolean-array half-n)]
      (loop [p 3]
        (when (< p sqrt-n)
          (when-not (aget primes (bit-shift-right p 1))
            (loop [i (long (bit-shift-right (* p p) 1))]
              (when (< i half-n)
                (aset primes i true)
                (recur (+ i p)))))
          (recur (+ p 2))))
      primes)))

(comment
  (defn loot [raw-sieve]
    (keep-indexed (fn [i v]
                    (when-not v (if (zero? i)
                                  2
                                  (inc (* i 2)))))
                  raw-sieve))
  (loot (sieve-ba 1))
  (loot (sieve-ba 10))
  (loot (sieve-ba 20))
  (loot (sieve-ba 100))
  (count (loot (sieve-ba 1000)))
  (count (loot (sieve-ba 1000000)))
  (with-progress-reporting (quick-bench (sieve-ba 1000000)))
  (time (do (sieve-ba 1000000) nil)))

(defn sieve-bs
  "Java BitSet storage
   Returns the raw sieve with only odd numbers present."
  [^long n]
  (if (< n 2)
    (java.util.BitSet. 0)
    (let [sqrt-n (unchecked-long (Math/ceil (Math/sqrt (double n))))
          half-n (unchecked-int (bit-shift-right n 1))
          primes (doto (java.util.BitSet. half-n) (.set (unchecked-int 0) half-n))]
      (loop [p 3]
        (when (< p sqrt-n)
          (when (.get primes (unchecked-int (bit-shift-right p 1)))
            (loop [i (bit-shift-right (* p p) 1)]
              (when (< i half-n)
                (.clear primes (unchecked-int i))
                (recur (+ i p)))))
          (recur (+ p 2))))
      primes)))

(defn sieve-bs-shroedinger
  "Java BitSet storage
   Returns the raw sieve with only odd numbers present
   This one runs faster than `sieve-bs` above, but there's
   something strange with this one. In 60-70% of the runs, 
   it runs at less than 0.5X it's max speed. In docker, and
   when on an intel Linux machine."
  [^long n]
  (if (< n 2)
    (java.util.BitSet. 0)
    (let [half-n (unchecked-int (bit-shift-right n 1))
          sqrt-n (unchecked-long (Math/ceil (Math/sqrt (double n))))
          primes (doto (java.util.BitSet. half-n) (.set (unchecked-int 0) half-n))]
      (loop [p 3]
        (when (< p sqrt-n)
          (when (.get primes (unchecked-long (bit-shift-right p 1)))
            (loop [i (bit-shift-right (* p p) 1)]
              (when (< i half-n)
                (.clear primes (unchecked-long i))
                (recur (+ i p)))))
          (recur (+ p 2))))
      primes)))

(comment
  (defn loot [raw-sieve]
    (map (fn [v]
           (if (zero? v)
             2
             (inc (* v 2))))
         (-> raw-sieve .stream .toArray)))
  (loot (sieve-bs 1))
  (loot (sieve-bs 10))
  (loot (sieve-bs 20))
  (loot (sieve-bs 100))
  (.cardinality (sieve-bs 100))
  (.cardinality (sieve-bs 1000))
  (.cardinality (sieve-bs 1000000))
  (with-progress-reporting (quick-bench (sieve-bs 1000000)))
  (time (do (sieve-bs 1000000) nil)))

(set! *unchecked-math* :warn-on-boxed)


(defn sieve-ba-to-vector-even-filter-futures
  "boolean-array-storage
   Returns the primes.
   We gain some time by skipping every other number while iterating.
   Then lose some time when filtering away the even numbers that we missed.
   The second step can be parallalized for some little speed gain.
   It all starts to make sense for larger sieves,
     at 1 million the gains are there, but small."
  [^long n]
  (if (< n 2)
    []
    (let [primes (boolean-array (inc n) true)
          sqrt-n (int (Math/ceil (Math/sqrt n)))]
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
  (sieve-ba-to-vector-even-filter-futures 1)
  ;; => []

  (sieve-ba-to-vector-even-filter-futures 10)
  ;; => [2 3 5 7]

  (sieve-ba-to-vector-even-filter-futures 100)
  ;; You try it!

  (with-progress-reporting (quick-bench (sieve-ba-to-vector-even-filter-futures 1000000)))
  (quick-bench (sieve-ba-to-vector-even-filter-futures 1000000))
  ;; Execution time mean : 2.703046 ms

  ;; This one takes a lot of time, you have been warned
  (with-progress-reporting (bench (sieve-ba-to-vector-even-filter-futures 1000000))))

(defn sieve-ba-all
  "boolean-array storage
   Returns the raw sieve.
   This version treats evens as equal candidates to any numbers."
  [^long n]
  (if (< n 2)
    (boolean-array n)
    (let [primes (boolean-array n true)
          sqrt-n (long (Math/ceil (Math/sqrt n)))]
      (aset primes 0 false)
      (aset primes 1 false)
      (loop [p 2]
        (if-not (< p sqrt-n)
          primes
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (< i n)
                  (aset primes i false)
                  (recur (+ i p)))))
            (recur (inc p))))))))

(defn sieve-ba-pre-even-filter
  "boolean-array storage
   Returns the raw sieve.
   Remove even indexes before sieving.
   No parallelisation."
  [^long n]
  (if (< n 2)
    (boolean-array n)
    (let [primes (boolean-array n true)
          sqrt-n (long (Math/ceil (Math/sqrt n)))]
      (aset primes 0 false)
      (aset primes 1 false)
      (loop [i 4]
        (when (< i n)
          (aset primes i false)
          (recur (+ i 2))))
      (loop [p 3]
        (if-not (< p sqrt-n)
          primes
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (< i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur (+ p 2))))))))

(defn sieve-ba-pre-even-filter-futures
  "boolean-array storage
   Returns the raw sieve.
   Remove even indexes before sieving.
   Same as `sieve-ba-pre-even-filter`,
     except we parallelize the first step with futures"
  [^long n]
  (if (< n 2)
    (boolean-array n)
    (let [primes (boolean-array n true)
          sqrt-n (long (Math/ceil (Math/sqrt n)))]
      (let
       [num-slices 8 ; I'm not sure what is a good default
        num-slices (if (and (zero? ^long (mod n num-slices))
                            (> n 1000))
                     num-slices
                     1)
        slice-size (quot n num-slices)
        futures (mapv (fn [^long slice-num]
                        (future
                          (let [start (* slice-num slice-size)
                                end (dec (+ start slice-size))]
                            (loop [i start]
                              (when (< i end)
                                (aset primes i false)
                                (recur (+ i 2)))))))
                      (range num-slices))]
        (doseq [f futures]
          (deref f)))
      (aset primes 1 false)
      (aset primes 2 true)
      (loop [p 3]
        (if-not (< p sqrt-n)
          primes
          (do
            (when (aget primes p)
              (loop [i (* p p)]
                (when (< i n)
                  (aset primes i false)
                  (recur (+ i p p)))))
            (recur (+ p 2))))))))

(comment
  ;; Evaluate the sieve you want to play with
  (def sieve sieve-ba-all)
  (def sieve sieve-ba-pre-even-filter)
  (def sieve sieve-ba-pre-even-filter-futures)

  ;; We get the raw sieve back, a Java boolean array
  (sieve 1)
  ;; => #object ["[Z" 0x816eab0 "[Z@816eab0"]

  ;; Most often you can treat it as a Clojure sequence/collection
  (first (sieve 1))
  ;; => false

  (nth (sieve 3) 2)
  ;; => true

  ;; Evaluate this one to use for emptying/looting the raw sieve
  (defn loot [raw-sieve]
    (keep-indexed (fn [i v] (when v i)) raw-sieve))

  (loot (sieve 1))
  ;; => ()

  (loot (sieve 10))
  ;; => (2 3 5 7)

  (-> 1000000
      sieve
      loot
      count)
  ;; => 78498

  (loot (sieve 100))
  ;; You try it!

  (with-progress-reporting (quick-bench (sieve 1000000)))
  (quick-bench (sieve 1000000))
  ;; Execution time mean : 1.563407 ms

  ;; This one takes a lot of time, you have been warned
  (with-progress-reporting (bench (sieve 1000000))))

(defn sieve-bs-all
  "BitSet storage.
   Returns the raw sieve.
   This version treats evens as equal candidates to any numbers."
  [^long n]
  (if (< n 2)
    (java.util.BitSet. n)
    (let [primes (doto (java.util.BitSet. n) (.set 2 n))
          sqrt-n (long (Math/ceil (Math/sqrt n)))]
      (loop [p 2]
        (if-not (< p sqrt-n)
          primes
          (do
            (when (.get primes p)
              (loop [i (* p p)]
                (when (< i n)
                  (.clear primes i)
                  (recur (+ i p)))))
            (recur (inc p))))))))

(defn sieve-bs-pre-even-filter
  "BitSet storage.
   Returns the raw sieve.
   Remove even indexes before sieving.
   No parallelisation."
  [^long n]
  (if (< n 2)
    (java.util.BitSet. n)
    (let [primes (doto (java.util.BitSet. n) (.set 2 n))
          sqrt-n (long (Math/ceil (Math/sqrt n)))]
      (loop [i 4]
        (when (< i n)
          (.clear primes i)
          (recur (+ i 2))))
      (loop [p 3]
        (if-not (< p sqrt-n)
          primes
          (do
            (when (.get primes p)
              (loop [i (* p p)]
                (when (< i n)
                  (.clear primes i)
                  (recur (+ i p p)))))
            (recur (+ p 2))))))))

(comment
  ;; Choose wisely
  (def sieve sieve-bs-all)
  (def sieve sieve-bs-pre-even-filter)

  (defn loot [raw-sieve]
    (map identity (-> raw-sieve .stream .toArray)))

  (loot (sieve 100))
  ;; We get the raw sieve back, a Java BitSet
  (sieve 1)
  ;; => #object [java.util.BitSet 0x222b4705 "{}"]

  (sieve 10)
  ;; => #object [java.util.BitSet 0x5793c7ae "{2, 3, 5, 7}"]

  (.cardinality (sieve 1000000))
  ;; => 78498

  (sieve 100)
  ;; You try it!

  (with-progress-reporting (quick-bench (sieve 1000000)))
  (quick-bench (sieve 1000000))
  ;; Execution time mean : 5.173709 ms

  ;; This one takes a lot of time, you have been warned
  (with-progress-reporting (bench (sieve 1000000)))
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
  [sieve]
  (let [limit       1000000
        start-time  (Instant/now)
        end-by      (+ (.toEpochMilli start-time) 5000)]
    (loop [pass 1]
      (let [primes   (sieve limit)
            cur-time (System/currentTimeMillis)]
        (if (<= cur-time end-by)
          (recur (inc pass))
          ;; Return benchmark report.
          {:primes primes
           :passes pass
           :limit  limit
           :time   (Duration/between start-time (Instant/now))})))))


;; Reenable overflow checks on mathematical ops and turn off warnings.
(set! *warn-on-reflection* false)
(set! *unchecked-math* false)


(defn format-results
  "Format benchmark results into expected output."
  [{:keys [primes passes limit time variant count-f threads bits]}]
  (let [nanos (.toString (.toNanos time))
        timef (str (subs nanos 0 1) "." (subs nanos 1))
        valid? (= (count-f primes)
                  (prev-results limit))]
    (str "Passes: " passes ", "
         "Time: " timef ", "
         "Avg: " (float (/ (/ (.toNanos time) 1000000000) passes)) ", "
         "Limit: " limit ", "
         "Count: " (count-f primes) ", "
         "Valid: " (if valid? "True" "False")
         "\n"
         "pez-clj-" (name variant) ";" passes ";" timef ";" threads ";algorithm=base,faithful=yes,bits=" bits)))

(def confs
  {:vector {:sieve sieve-v
            :count-f (fn [primes] (count (filter true? primes)))
            :threads 1
            :bits "?"}
   :vector-transient {:sieve sieve-v-transient
                      :count-f (fn [primes] (count (filter true? primes)))
                      :threads 1
                      :bits "?"}
   :bitset {:sieve sieve-bs
            :count-f (fn [primes] (.cardinality primes))
            :threads 1
            :bits 1}
   :bitset-shroedinger {:sieve sieve-bs-shroedinger
                        :count-f (fn [primes] (.cardinality primes))
                        :threads 1
                        :bits 1}
   :bitset-all {:sieve sieve-bs-all
                :count-f (fn [primes] (.cardinality primes))
                :threads 1
                :bits 1}
   :bitset-pre {:sieve sieve-bs-pre-even-filter
                :count-f (fn [primes] (.cardinality primes))
                :threads 1
                :bits 1}
   :boolean-array {:sieve sieve-ba
                   :count-f (fn [primes] (count (filter false? primes)))
                   :threads 1
                   :bits 8}
   :boolean-array-all {:sieve sieve-ba-all
                       :count-f (fn [primes] (count (filter true? primes)))
                       :threads 1
                       :bits 8}
   :boolean-array-pre {:sieve sieve-ba-pre-even-filter
                       :count-f (fn [primes] (count (filter true? primes)))
                       :threads 1
                       :bits 8}
   :boolean-array-pre-futures {:sieve sieve-ba-pre-even-filter-futures
                               :count-f (fn [primes] (count (filter true? primes)))
                               :threads 8
                               :bits 8}
   :boolean-array-to-vector-futures {:sieve sieve-ba-to-vector-even-filter-futures
                                     :count-f count
                                     :threads 8
                                     :bits 8}})

(defn run [{:keys [variant warm-up?]
            :or   {variant :boolean-array
                   warm-up? false}}]
  (let [conf (confs variant)
        sieve (:sieve conf)]
    (when warm-up?
      ;; Warm-up reduces the variability of results.
      (format-results (merge conf (benchmark sieve) {:variant variant})))
    (println (format-results (merge conf (benchmark sieve) {:variant variant})))))

(defn -main [& args]
  (run (clojure.edn/read-string (first args))))

(comment
  (run {:warm-up? true})
  (run {:warm-up? false})
  (run {:variant :vector :warm-up? true})
  (run {:variant :vector-transient :warm-up? true})
  (run {:variant :bitset :warm-up? true})
  (run {:variant :bitset-shroedinger :warm-up? true})
  (run {:variant :bitset :warm-up? true})
  (run {:variant :bitset-all :warm-up? true})
  (run {:variant :bitset-pre :warm-up? true})
  (run {:variant :boolean-array :warm-up? true})
  (run {:variant :boolean-array-all :warm-up? true})
  (run {:variant :boolean-array-pre :warm-up? true})
  (run {:variant :boolean-array-pre-futures :warm-up? true})
  (run {:variant :boolean-array-to-vector-futures :warm-up? true})
  )