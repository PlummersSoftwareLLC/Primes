;;;; based on mikehw's solution, approx. 120x speedup
;;;
;;; run as:
;;;     sbcl --script PrimeSieve.lisp
;;;

(declaim
  (optimize (speed 3) (safety 0) (debug 0) (space 0))
  (inline get-bit)
  (inline clear-bit))


(defun get-bit (rawbits index)
  (declare (simple-bit-vector rawbits))
  (if (zerop (mod index 2))
        0
    (aref rawbits
        (floor index 2))))

(defun clear-bit (rawbits index)
  (declare (simple-bit-vector rawbits))
  (setf (aref rawbits (floor index 2)) 0))

(defun run-sieve (sieve-size)
  (declare (fixnum sieve-size))

  (let ((rawbits (make-array
                   (floor (1+ sieve-size) 2)
                   :element-type 'bit
                   :initial-element 1))
        (q (floor (sqrt sieve-size))))
    (declare (fixnum q))
    (do ((factor 3 (+ factor 2))) ((> factor q))
      (declare (fixnum factor))

      (loop for num fixnum
            from factor
            to sieve-size
            until (when (= (get-bit rawbits num) 1)
                    (setq factor num)
                    t))

      (loop for num fixnum
            from (the fixnum (* factor factor))
            to sieve-size
            by (the fixnum (* factor 2))  do
        (clear-bit rawbits num)))
    rawbits))

(defun count-primes (rawbits)
  (reduce #'+ rawbits))

(let* ((passes 0)
       (start (get-internal-real-time))
       (end (+ start (* internal-time-units-per-second 5)))
       result)

  (do () ((>= (get-internal-real-time) end))
    (setq result (run-sieve 1000000))
    (incf passes))

  (let* ((duration  (/ (- (get-internal-real-time) start) internal-time-units-per-second))
         (avg (/ duration passes)))
    (format *error-output* "Passes: ~d  Time: ~f Avg: ~f ms Count: ~d~%" passes duration (* 1000 avg)  (count-primes result))

    (format t "mayerrobert-cl;~d;~f;1;algorithm=base,faithful=no,bits=1~%" passes duration)))
