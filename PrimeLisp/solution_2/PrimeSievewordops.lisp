;;;; based on sieve_1of2.c by  by Daniel Spangberg, but sets bits one word at a time
;;;
;;; run as:
;;;     sbcl --script PrimeSievewordops.lisp
;;;


(declaim
  (optimize (speed 3) (safety 0) (debug 0) (space 0))

  (inline nth-bit-set-p)
  (inline set-nth-bit)
  (inline set-bits))


(defparameter *list-to* 100
  "list primes up to that number, set to nil to disable listing")


(defconstant +results+
  '((         10 . 4        )
    (        100 . 25       )
    (        127 . 31       )
    (        128 . 31       )
    (        129 . 31       )
    (       1000 . 168      )
    (      10000 . 1229     )
    (     100000 . 9592     )
    (    1000000 . 78498    )
    (   10000000 . 664579   ))
  "Historical data for validating our results - the number of primes
   to be found under some limit, such as 168 primes under 1000")


#+64-bit (defconstant +bits-per-word+ 64)
#-64-bit (defconstant +bits-per-word+ 32)

(deftype sieve-element-type ()
  `(unsigned-byte ,+bits-per-word+))

(deftype sieve-array-type ()
  `(simple-array sieve-element-type 1))

(deftype sieve-bitpos-type ()
  `(integer 0 ,(1- +bits-per-word+)))


(defclass sieve-state ()
  ((maxints :initarg :maxints
            :type fixnum
            :accessor sieve-state-maxints)

   (a       :initarg :a
            :type sieve-array-type
            :accessor sieve-state-a)))


(defun create-sieve (maxints)
  (declare (fixnum maxints))
  (make-instance 'sieve-state
    :maxints maxints
    :a (make-array (ceiling (ceiling maxints +bits-per-word+) 2)
         :element-type 'sieve-element-type
         :initial-element 0)))


(defmacro shl (x bits)
  `(ldb (byte 64 0) (ash ,x (the sieve-bitpos-type ,bits))))


(defun nth-bit-set-p (a n)
  "Returns t if n-th bit is set in array a, nil otherwise."
  (declare (sieve-array-type a)
           (fixnum n))
  (multiple-value-bind (q r) (floor n +bits-per-word+)
    (declare (fixnum q r))
    (logbitp r (aref a q))))

(defun set-nth-bit (a n)
  "Set n-th bit in array a to 1."
  (declare (type sieve-array-type a)
           (type fixnum n))
  (multiple-value-bind (q r) (floor n +bits-per-word+)
    (declare (fixnum q r))
    (setf #1=(aref a q)
         (logior #1# (expt 2 r)))) 0)


(eval-when (:load-toplevel :compile-toplevel :execute)

(defun patterns ()
  "Create a vector of bit-patterns."
  (labels ((pattern (n)
             "Return a bit pattern where every n-th bit is 1, starting from least significant bit."
             (let ((result 0))
               (declare (sieve-element-type result))
               (loop for b fixnum from (1- +bits-per-word+) downto 0 do
                 (if (zerop (mod b n))
                       (setq result (logior (ash result 1) 1))
                   (setq result (ash result 1))))
               result)))

    (let ((res (make-array 33 :element-type 'sieve-element-type :initial-element 0)))
      (loop for x fixnum
            from 2
            to 32
            do (setf (aref res x) (pattern x)))
      res)))

) ; end eval-when


(defconstant +patterns+ (coerce (patterns) '(simple-array sieve-element-type 1))
  "A vector of bit pattern where every n-th bit is 1, starting from least significant bit.
E.g. (aref +patterns+ 7) is a bitpattern with every 7th bit set.")


(defun set-bits (bits first-incl last-excl every-nth)
  "Set every every-nth bit in array bits between first-incl and last-excl."
  (declare (type fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))
  (if (<= every-nth 32)

        (let ((pattern (aref +patterns+ every-nth)) (shift 0) (total 0))
          (declare (type sieve-element-type pattern) (fixnum shift total))

          ; set first word and prepare shift amounts
          (multiple-value-bind (q r) (floor first-incl +bits-per-word+)
            (when (< last-excl +bits-per-word+)
              (setf (aref bits q) (logior (aref bits q)
                                          (logand (shl pattern r)
                                                  (1- (ash 1 (the sieve-bitpos-type last-excl))))))
              (return-from set-bits nil))

            (setf (aref bits q) (logior (aref bits q) (shl pattern r)))
            (setq total (mod r every-nth))
            (setq shift (- every-nth (mod +bits-per-word+ every-nth))))

          ; set remaining words
          (loop with tmp of-type fixnum
                for num of-type fixnum
                from (1+ (floor first-incl +bits-per-word+))
                below (floor last-excl +bits-per-word+)
                do
                  (when (>= (setq total (+ total shift)) every-nth)
                    (setq total (- total every-nth)))

                  (setf (aref bits num) (logior (aref bits num) (shl pattern total)))

                finally ; set last word
                  (setq tmp (- last-excl (* num +bits-per-word+)))
                  (when (> tmp 0)
                    (when (>= (setq total (+ total shift)) every-nth)
                      (setq total (- total every-nth)))

                    (setf (aref bits num) (logior (aref bits num)
                                                  (logand (shl pattern total)
                                                          (1- (ash 1 (the sieve-bitpos-type tmp)))))))))

    (loop for num of-type fixnum
          from first-incl
          below last-excl
          by every-nth
          do
      (set-nth-bit bits num))))


(defun run-sieve (sieve-state)
  (declare (type sieve-state sieve-state))

  (let* ((rawbits (sieve-state-a sieve-state))
         (sieve-size (sieve-state-maxints sieve-state))
         (sieve-sizeh (ceiling sieve-size 2))
         (factor 0)
         (factorh 1)
         (qh (ceiling (isqrt sieve-size) 2)))
    (declare (fixnum sieve-size sieve-sizeh factor factorh qh) (type sieve-array-type rawbits))
    (loop do

      (loop for num of-type fixnum
            from factorh
            to qh
            while (nth-bit-set-p rawbits num)
            finally (setq factor (1+ (* num 2)))
                    (setq factorh (1+ num)))

      (when (> factorh qh)
        (return-from run-sieve sieve-state))

      (set-bits rawbits (floor (the fixnum (* factor factor)) 2) sieve-sizeh factor))))


(defun count-primes (sieve-state)
  (declare (sieve-state sieve-state))
  (let ((max (sieve-state-maxints sieve-state))
        (bits (sieve-state-a sieve-state))
        (result 0))
    (declare (fixnum result))
    (loop for i fixnum
          from 1
          to max
          by 2
          do
      (unless (nth-bit-set-p bits (floor i 2))
        (incf result)))
    result))


(defun list-primes (result)
  (princ "2, " *error-output*)
  (let ((bits (sieve-state-a result)))
    (loop for i
          from 3
          to (min *list-to* (sieve-state-maxints result))
          by 2 do
      (unless  (nth-bit-set-p bits (floor i 2))
        (format *error-output* "~d, " i))))
  (when (< *list-to* (sieve-state-maxints result))
    (princ "..." *error-output*))
  (terpri *error-output*))


(defun test ()
  "Run run-sieve on all historical data in +results+, return nil if there is any deviation."
  (let ((result t))
    (mapc #'(lambda (tupel)
              (unless (= (cdr tupel) (count-primes (run-sieve (create-sieve (car tupel)))))
                (format *error-output* "ERROR: ~d produces wrong result~%" (car tupel))
                (setq result nil)))
            +results+)
    result))


(defun validate (sieve-state)
  "Invoke test, and then check if sieve-state is correct
according to the historical data in +results+."
  (let ((hist (cdr (assoc (sieve-state-maxints sieve-state) +results+ :test #'=))))
    (if (and (test) hist (= (count-primes sieve-state) hist)) "yes" "no")))


(let* ((passes 0)
       (start (get-internal-real-time))
       (end (+ start (* internal-time-units-per-second 5)))
       result)
  (declare (fixnum passes))

  (loop while (<= (get-internal-real-time) end)
        do (setq result (run-sieve (create-sieve 1000000)))
           (incf passes))

  (let* ((duration  (/ (- (get-internal-real-time) start) internal-time-units-per-second))
         (avg (/ duration passes)))
    (when *list-to* (list-primes result))
    (format *error-output* "Algorithm: base w/ wordops  Passes: ~d  Time: ~f Avg: ~f ms Count: ~d  Valid: ~A~%"
            passes duration (* 1000 avg) (count-primes result) (validate result))

    (format t "mayerrobert-cl-words;~d;~f;1;algorithm=other,faithful=yes,bits=1~%" passes duration)))
