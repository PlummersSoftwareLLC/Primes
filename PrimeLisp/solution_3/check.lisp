;;;; SPDX-License-Identifier: BSD-3-Clause
;; Independent byte-array sieve; no generated kernels or packed masks.
(in-package #:prime-bench)
(declaim (optimize (speed 1) (safety 3) (debug 2)))

(defun oracle (limit)
  (let ((flags (make-array (1+ limit) :element-type '(unsigned-byte 8)
                          :initial-element 1)))
    (setf (aref flags 0) 0)
    (when (>= limit 1) (setf (aref flags 1) 0))
    (loop for p from 2 to (isqrt limit) when (= 1 (aref flags p)) do
      (loop for n from (* p p) to limit by p do (setf (aref flags n) 0)))
    flags))

(defun check-limit (limit)
  (let ((expected (oracle limit)))
    (pm:with-sieve (state limit)
      (pm:run-sieve state)
      (assert (= (count 1 expected) (pm:count-primes state)))
      (loop for n from 0 to limit do
        (assert (eq (= 1 (aref expected n)) (not (null (pm:primep state n))))))
      (assert (not (pm:primep state (1+ limit))))
      (pm:with-sieve (other 17)
        (pm:run-sieve other)
        (assert (= 7 (pm:count-primes other)))
        (assert (= (count 1 expected) (pm:count-primes state)))))))

(defun check-guards (limit)
  (let* ((nwords (ceiling (ceiling limit 2) 64)) (nbytes (* nwords 8))
         (raw (pm::native-malloc (+ nbytes 128))))
    (assert (plusp raw))
    (unwind-protect
         (let ((state (pm::make-foreign-state
                       :limit limit :nwords nwords :address (+ raw 64))))
           (declare (dynamic-extent state))
           (pm::native-memset raw 90 (+ nbytes 128))
           (pm::native-memset (+ raw 64) 0 nbytes)
           (when (plusp nwords)
             (setf (sb-sys:sap-ref-64 (sb-sys:int-sap (+ raw 64)) 0) 1))
           (pm:run-sieve state)
           (let ((sap (sb-sys:int-sap raw)))
             (dotimes (i 64)
               (assert (= 90 (sb-sys:sap-ref-8 sap i)))
               (assert (= 90 (sb-sys:sap-ref-8 sap (+ nbytes 64 i))))))
           (assert (= (count 1 (oracle limit)) (pm:count-primes state))))
      (pm::native-free raw))))

(dolist (limit '(0 1 2 3 4 9 25 49 63 64 65 127 128 129 255 256 257
                511 512 513 961 1023 1024 1025 3969 4095 4096 4097
                16129 16383 16384 16385 65025 65535 65536 65537
                99991 100000 524287 524288 524289 999983 999999
                1000000 1000001 1042441 1048575 1048576 1048577 2000003))
  (check-limit limit))
(dolist (limit '(0 1 127 128 129 16129 524287 524288 524289
                1048575 1048576 1048577 2000003))
  (check-guards limit))
(pm:with-sieve (state 10000000)
  (pm:run-sieve state)
  (sb-ext:gc :full t)
  (assert (= 664579 (pm:count-primes state))))
(format *error-output* "Full flags, block boundaries, lifetimes and memory guards: Pass~%")
