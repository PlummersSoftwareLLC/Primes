;;;; based on https://github.com/PlummersSoftwareLLC/Primes/pull/641
;;;
;;; run as:
;;;     sbcl --script PrimeSieveModulo.lisp
;;;


(declaim
  (optimize (speed 3) (safety 0) (debug 0) (space 0))

  (inline or-word)
  (inline nth-bit-set-p)
  (inline set-nth-bit)

  (inline make-index)
  (inline set-bits-simple)
  (inline set-bits-unrolled)
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


(defconstant +bits-per-word+ 8)

(deftype nonneg-fixnum ()
  `(integer 0 ,most-positive-fixnum))
  ;`(unsigned-byte 64))

(deftype sieve-element-type ()
  `(unsigned-byte ,+bits-per-word+))

(deftype sieve-array-type ()
  `(simple-array sieve-element-type 1))


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


(defun nth-bit-set-p (a n)
  "Returns t if n-th bit is set in array a, nil otherwise."
  (declare (sieve-array-type a)
           (nonneg-fixnum n))
  (multiple-value-bind (q r) (floor n +bits-per-word+)
    (declare (nonneg-fixnum q r))
    (logbitp r (aref a q))))


(defun or-word (a idx pattern)
  (declare (type sieve-array-type a)
           (type nonneg-fixnum idx)
           (type sieve-element-type pattern))
  (setf #1=(aref a idx) (logior #1# pattern)))


(defun set-nth-bit (a n)
  "Set n-th bit in array a to 1."
  (declare (type sieve-array-type a)
           (type nonneg-fixnum n))
  (multiple-value-bind (q r) (floor n +bits-per-word+)
    (declare (nonneg-fixnum q r))
    (or-word a q (expt 2 r)))
  0)


(defun set-bits-simple (bits first-incl last-excl every-nth)
  (declare (type nonneg-fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))
  (loop while (< first-incl last-excl)
        do (set-nth-bit bits first-incl)
           (incf first-incl every-nth)))


(defun set-bits-unrolled (bits first-incl last-excl every-nth)
  "Use an unrolled loop to set every every-th bit to 1"
  (declare (type nonneg-fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))
  (let* ((i first-incl)
         (every-nth-times-2 (+ every-nth every-nth))
         (every-nth-times-3 (+ every-nth-times-2 every-nth))
         (every-nth-times-4 (+ every-nth-times-3 every-nth)))
    (declare (nonneg-fixnum i every-nth-times-2 every-nth-times-3 every-nth-times-4))

    (when (> last-excl (the nonneg-fixnum (+ i every-nth-times-4)))
      (loop with end1 of-type nonneg-fixnum = (- last-excl every-nth-times-4)
            while (< i end1)
            do (set-nth-bit bits i)
               (set-nth-bit bits (+ i every-nth))
               (set-nth-bit bits (+ i every-nth-times-2))
               (set-nth-bit bits (+ i every-nth-times-3))
               (incf i every-nth-times-4)))

    (set-bits-simple bits i last-excl every-nth)))


(eval-when (:load-toplevel :compile-toplevel :execute)

(defun sym (s1 s2)
  (intern (format nil "~A~A" s1 s2)))


(defun generate-x-y-loop (startmod skipmod)
  `(loop ,@(loop for n from 0 below +bits-per-word+
                 append `(,(if (zerop n) 'with 'and) ,(sym "C" n) of-type nonneg-fixnum = (floor (the nonneg-fixnum (+ ,startmod (the nonneg-fixnum (* ,n every-nth)))) +bits-per-word+)))
         for word of-type nonneg-fixnum
         from bulkstartword
         below bulkendword
         by every-nth
         do (progn ,@(loop for n from 0 below +bits-per-word+
                    collect `(or-word bits (+ word ,(sym "C" n)) ,(ash 1 (mod (+ startmod (* n skipmod)) +bits-per-word+)))))
         finally (setq first-incl (+ ,startmod (the nonneg-fixnum (* word +bits-per-word+))))))


(defun make-index (startmod skipmod)
  (declare (type nonneg-fixnum startmod skipmod))
  (the nonneg-fixnum (+ (floor startmod 2) (ash (floor skipmod 2) 2))))


(defmacro generate-ecase ()
  ; first-incl is even, every-nth is odd, see comment in run-sieve below.
  ; therefore only a few combinations can happen: startmod [0 2 4 6] and skipmod [1 3 5 7]
  ; if the ecase keys are small (and maybe dense?) fixnums then SBCL will compile the ecase into a jump table
  `(ecase (the nonneg-fixnum (make-index startmod skipmod))
     ,@(loop for y from 1 below +bits-per-word+ by 2
             append (loop for x from 0 below +bits-per-word+ by 4
                          collect `(,(make-index x y)
                                    ,(generate-x-y-loop x y)
                                    (set-bits-simple bits first-incl last-excl every-nth))))))

) ; end eval-when


(defun set-bits (bits first-incl last-excl every-nth)
  "Set every every-nth bit in array bits between first-incl and last-excl."
  (declare (type nonneg-fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))

  (let* ((bulkstartword (floor first-incl +bits-per-word+))
         (bulkstart     (* bulkstartword +bits-per-word+)))
    (declare (nonneg-fixnum bulkstartword bulkstart))

    (if (and (> last-excl (the nonneg-fixnum (* +bits-per-word+ every-nth)))
             (< bulkstart last-excl))

          (let ((startmod (mod first-incl +bits-per-word+))
                (skipmod (mod every-nth +bits-per-word+))
                (bulkendword (floor (the nonneg-fixnum (- last-excl (the nonneg-fixnum (* +bits-per-word+ every-nth)))) +bits-per-word+)))
            (declare (nonneg-fixnum startmod skipmod bulkendword))

            (generate-ecase))

      (set-bits-unrolled bits first-incl last-excl every-nth))))


(defun run-sieve (sieve-state)
  (declare (type sieve-state sieve-state))

  (let* ((rawbits (sieve-state-a sieve-state))
         (sieve-size (sieve-state-maxints sieve-state))
         (sieve-sizeh (ceiling sieve-size 2))
         (factor 0)
         (factorh 1)
         (qh (floor (the nonneg-fixnum (1+ (isqrt sieve-size))) 2)))
    (declare (nonneg-fixnum sieve-size sieve-sizeh factor factorh qh) (type sieve-array-type rawbits))
    (loop do

      (loop for num of-type nonneg-fixnum
            from factorh
            to qh
            while (nth-bit-set-p rawbits num)
            finally (setq factor (1+ (* num 2)))
                    (setq factorh (1+ num)))

      (when (> factorh qh)
        (return-from run-sieve sieve-state))

      ; factor is an odd number >= 3
      ; (floor (the fixnum (* factor factor)) 2) evals to a multiple of 4
      (set-bits rawbits (floor (the nonneg-fixnum (* factor factor)) 2) sieve-sizeh factor))))


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
    (format *error-output* "Algorithm: base w/ modulo  Passes: ~d  Time: ~f Avg: ~f ms Count: ~d  Valid: ~A~%"
            passes duration (* 1000 avg) (count-primes result) (validate result))

    (format t "mayerrobert-cl-modulo;~d;~f;1;algorithm=base,faithful=yes,bits=1~%" passes duration)))


; uncomment the following line to display the generated loop stmt for setting every 3rd bit starting at 4
;(format *error-output* "The bit-setting loop for startmod=4 and skipmod=3:~%~A~%" (generate-x-y-loop 4 3))


; uncomment the following line to display the generated ecase stmt containing all bit-setting loops
;(format *error-output* "Expansion of macro generate-ecase:~%~A~%" (macroexpand-1 '(generate-ecase)))
