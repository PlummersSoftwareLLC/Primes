;;;; based on mikehw's solution, approx. 220x speedup
;;;
;;; run as:
;;;     sbcl --script PrimeSieve.lisp
;;;


#+(and :sbcl :x86-64)
(when (equalp "2.0.0" (lisp-implementation-version))
  (load "bitvector-set-2.0.0-2.1.8-snap.lisp")) ; teach sbcl 2.0.0 the new bitvector-set from sbcl 2.1.8


(declaim
  (optimize (speed 3) (safety 0) (debug 0) (space 0)))


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


(deftype number-t ()
  `(integer 0 ,most-positive-fixnum))
  ;`(unsigned-byte 64))


(defclass sieve-state ()
  ((maxints :initarg :maxints
            :type number-t
            :accessor sieve-state-maxints)

   (a       :initarg :a
            :type simple-bit-vector
            :accessor sieve-state-a)))


(defun create-sieve (maxints)
  (declare (type number-t maxints))
  (make-instance 'sieve-state
    :maxints maxints
    :a (make-array (ceiling maxints 2)
         :element-type 'bit
         :initial-element 0)))


(defun run-sieve (sieve-state)
  (declare (type sieve-state sieve-state))

  (loop with rawbits    of-type simple-bit-vector  = (sieve-state-a sieve-state)
        with sieve-size of-type number-t = (sieve-state-maxints sieve-state)
        with q          of-type number-t = (the number-t (isqrt sieve-size))
        with end        of-type number-t = (floor (the number-t (1+ sieve-size)) 2) ; ceiling with (unsigned-byte 64) gives slow code
        with factor     of-type number-t = 3

        while (<= factor q)

        do ; (position 0 bitvector :start pos) finds the index of the first
           ; 0-bit starting at pos
           (setq factor (1+ (* 2 (position 0 rawbits :start (floor factor 2)))))

           (let* ((i              (floor (the number-t (* factor factor)) 2))
                  (factor-times-2 (+ factor factor))
                  (factor-times-3 (+ factor-times-2 factor))
                  (factor-times-4 (+ factor-times-3 factor)))
             (declare (type number-t i factor-times-2 factor-times-3 factor-times-4))

             ; use an unrolled loop to set every factor-th bit to 1
             (when (> end (the number-t (+ i factor-times-4)))
               (loop with end1 of-type number-t = (- end factor-times-4)
                     while (< i end1)
                     do (setf (sbit rawbits i) 1)
                        (setf (sbit rawbits (+ i factor)) 1)
                        (setf (sbit rawbits (+ i factor-times-2)) 1)
                        (setf (sbit rawbits (+ i factor-times-3)) 1)
                        (incf i factor-times-4)))

             (loop while (< i end)
                   do (setf (sbit rawbits i) 1)
                      (incf i factor)))

           (incf factor 2))
  sieve-state)


(defun count-primes (sieve-state)
  (reduce #'+ (sieve-state-a sieve-state) :key (lambda (n) (- 1 n))))


(defun list-primes (result)
  (princ "2, " *error-output*)
  (let ((bits (sieve-state-a result)))
    (loop for i
          from 3
          to (min *list-to* (sieve-state-maxints result))
          by 2 do
      (when (zerop (sbit bits (floor i 2)))
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
  (declare (number-t passes))

  (loop while (<= (get-internal-real-time) end)
        do (setq result (run-sieve (create-sieve 1000000)))
           (incf passes))

  (let* ((duration  (/ (- (get-internal-real-time) start) internal-time-units-per-second))
         (avg (/ duration passes)))
    (when *list-to* (list-primes result))
    (format *error-output* "Algorithm: base  Passes: ~d  Time: ~f  Avg: ~f ms  Count: ~d  Valid: ~A~%"
            passes duration (* 1000 avg) (count-primes result) (validate result))

    (format t "mayerrobert-cl;~d;~f;1;algorithm=base,faithful=yes,bits=1~%" passes duration)))


; Same timed loop again, this time there is "#." before the invocation of run-sieve.
; See http://clhs.lisp.se/Body/02_dh.htm for what #. does.
(let* ((passes 0)
       (start (get-internal-real-time))
       (end (+ start (* internal-time-units-per-second 5)))
       result)
  (declare (number-t passes))

  (loop while (<= (get-internal-real-time) end)
        do (setq result #. (run-sieve (create-sieve 1000000)))
           (incf passes))

  (let* ((duration  (/ (- (get-internal-real-time) start) internal-time-units-per-second))
         (avg (/ duration passes)))
    (when *list-to* (list-primes result))
    (format *error-output* "Algorithm: base  Passes: ~d  Time: ~f  Avg: ~f ms  Count: ~d  Valid: ~A~%"
            passes duration (* 1000 avg) (count-primes result) (validate result))

    (format t "mayerrobert-cl-hashdot;~d;~f;1;algorithm=base,faithful=no,bits=1~%" passes duration)))
