;;;; based on sieve_1of2.c by  by Daniel Spangberg, but sets bits one word at a time
;;;
;;; run as:
;;;     sbcl --script PrimeSievewordops.lisp
;;;


(declaim
  (optimize (speed 3) (safety 0) (debug 0) (space 0))

  (inline shl)
  (inline bit-pattern)
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
    (   10000000 . 664579   )
    ;(  100000000 . 5761455  )
    ;( 1000000000 . 50847534 )
    ;(10000000000 . 455052511)
    )
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
    :a (make-array (1+ (floor (floor maxints +bits-per-word+) 2))
         :element-type 'sieve-element-type
         :initial-element 0)))


(defun shl (x bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (declare (type sieve-element-type x) (type sieve-bitpos-type bits))
  (logand (ash x bits)
          (1- (ash 1 +bits-per-word+))))


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


(defun bit-pattern (n)
  "Return a 64bit pattern where every n-th bit is 1, starting from least significant bit."
  (declare (fixnum n))
  (logand (1- (ash 1 +bits-per-word+))
    (case  n
      (2  #b0101010101010101010101010101010101010101010101010101010101010101)
      (3  #b1001001001001001001001001001001001001001001001001001001001001001)
      (4  #b0001000100010001000100010001000100010001000100010001000100010001)
      (5  #b0001000010000100001000010000100001000010000100001000010000100001)
      (6  #b0001000001000001000001000001000001000001000001000001000001000001)
      (7  #b1000000100000010000001000000100000010000001000000100000010000001)
      (8  #b0000000100000001000000010000000100000001000000010000000100000001)
      (9  #b0100000001000000001000000001000000001000000001000000001000000001)
      (10 #b0001000000000100000000010000000001000000000100000000010000000001)
      (11 #b0000000010000000000100000000001000000000010000000000100000000001)
      (12 #b0001000000000001000000000001000000000001000000000001000000000001)
      (13 #b0000000000010000000000001000000000000100000000000010000000000001)
      (14 #b0000000100000000000001000000000000010000000000000100000000000001)
      (15 #b0001000000000000001000000000000001000000000000001000000000000001)
      (16 #b0000000000000001000000000000000100000000000000010000000000000001)
      (17 #b0000000000001000000000000000010000000000000000100000000000000001)
      (18 #b0000000001000000000000000001000000000000000001000000000000000001)
      (19 #b0000001000000000000000000100000000000000000010000000000000000001)
      (20 #b0001000000000000000000010000000000000000000100000000000000000001)
      (21 #b1000000000000000000001000000000000000000001000000000000000000001)
      (22 #b0000000000000000000100000000000000000000010000000000000000000001)
      (23 #b0000000000000000010000000000000000000000100000000000000000000001)
      (24 #b0000000000000001000000000000000000000001000000000000000000000001)
      (25 #b0000000000000100000000000000000000000010000000000000000000000001)
      (26 #b0000000000010000000000000000000000000100000000000000000000000001)
      (27 #b0000000001000000000000000000000000001000000000000000000000000001)
      (28 #b0000000100000000000000000000000000010000000000000000000000000001)
      (29 #b0000010000000000000000000000000000100000000000000000000000000001)
      (30 #b0001000000000000000000000000000001000000000000000000000000000001)
      (31 #b0100000000000000000000000000000010000000000000000000000000000001)
      (32 #b0000000000000000000000000000000100000000000000000000000000000001)
      )))


(defun set-bits (bits first-incl last-excl every-nth)
  "Set every every-nth bit in array bits between first-incl and last-excl."
  (declare (type fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))
  (if (<= every-nth 32)

        (let* ((pattern (bit-pattern every-nth)) (tmp 0) (shift 0) (total 0))
          (declare (type sieve-element-type pattern) (fixnum tmp shift total))

          ; set first word and prepare pattern
          (multiple-value-bind (q r) (floor first-incl +bits-per-word+)
            (when (< last-excl +bits-per-word+)
              (setf (aref bits q) (logior (aref bits q)
                                          (logand (shl pattern r)
                                                  (1- (shl 1 last-excl)))))
              (return-from set-bits nil))

            (setf (aref bits q) (logior (aref bits q) (shl pattern r)))
            (setq total (mod r every-nth))
            (setq pattern (shl pattern total))
            (setq shift (- every-nth (mod +bits-per-word+ every-nth))))

          ; set remaining words
          (loop for num of-type fixnum
                from (1+ (floor first-incl +bits-per-word+))
                to (1- (floor last-excl +bits-per-word+))
                do
                  (if (>= (setq total (+ total shift)) every-nth)
                        (progn
                          (setq pattern (logior (shl pattern shift) (shl 1 (- total every-nth))))
                          (setq total (- total every-nth)))
                    (setq pattern (shl pattern shift)))

                  (setf (aref bits num) (logior (aref bits num) pattern))

                finally ; set last word
                  (setq tmp (- last-excl (* num +bits-per-word+)))
                  (when (> tmp 0)
                    (if (>= (setq total (+ total shift)) every-nth)
                          (setq pattern (logior (shl pattern shift) (shl 1 (- total every-nth))))
                      (setq pattern (shl pattern shift)))

                    ; adjust pattern so that only up to last-excl bits will be changed
                    (setq pattern (logand pattern (1- (shl 1 tmp))))

                    (setf (aref bits num) (logior (aref bits num) pattern)))))

    (loop for num of-type fixnum
          from first-incl
          to (1- last-excl)
          by every-nth
          do
      (set-nth-bit bits num))))


(defun run-sieve (sieve-state)
  (declare (type sieve-state sieve-state))

  (let* ((rawbits (sieve-state-a sieve-state))
         (sieve-size (sieve-state-maxints sieve-state))
         (sieve-sizeh (ceiling sieve-size 2))
         (qh (ceiling (floor (sqrt sieve-size)) 2)))
    (declare (fixnum sieve-size sieve-sizeh qh) (type sieve-array-type rawbits))
    (do ((factor 3)
         (factorh 1))
        (nil)
      (declare (fixnum factor factorh))

      (loop for num of-type fixnum
            from factorh
            to qh
            while (nth-bit-set-p rawbits num)
            finally (setq factor (1+ (* num 2)))
                    (setq factorh (1+ num)))

      (when (> factorh qh)
        (return-from run-sieve sieve-state))

      (set-bits rawbits (floor (the fixnum (* factor factor)) 2) sieve-sizeh factor))
    sieve-state))


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
    (if (and (test) hist (= hist (count-primes sieve-state))) "yes" "no")))


(let* ((passes 0)
       (start (get-internal-real-time))
       (end (+ start (* internal-time-units-per-second 5)))
       result)
  (declare (fixnum passes))

  (do () ((>= (get-internal-real-time) end))
    (setq result (create-sieve 1000000))
    (run-sieve result)
    (incf passes))

  (let* ((duration  (/ (- (get-internal-real-time) start) internal-time-units-per-second))
         (avg (/ duration passes)))
    (when *list-to* (list-primes result))
    (format *error-output* "Algorithm: base w/ wordops  Passes: ~d  Time: ~f Avg: ~f ms Count: ~d  Valid: ~A~%"
            passes duration (* 1000 avg) (count-primes result) (validate result))

    (format t "mayerrobert-cl-words;~d;~f;1;algorithm=base,faithful=yes,bits=1~%" passes duration)))
