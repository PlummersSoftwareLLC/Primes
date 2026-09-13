;;;; SPDX-License-Identifier: BSD-3-Clause
(defpackage #:prime-bench (:use #:cl) (:export #:check #:measure))
(in-package #:prime-bench)
(declaim (optimize (speed 3) (safety 1) (debug 1)))

(defun result-label ()
  ;; Attribution belongs to the benchmark output, not to the sieve implementation.
  (with-open-file (author "author.txt")
    (format nil "~(~A~)-~A" (read-line author) pm:*name*)))

(defun trial-prime-p (n)
  (and (>= n 2) (loop for d from 2 to (isqrt n) never (zerop (mod n d)))))

(defun check ()
  (dolist (limit '(0 1 2 3 4 9 25 49 63 64 65 127 128 129 255 256 257 1023))
    (pm:with-sieve (state limit)
      (pm:run-sieve state)
      (loop for n from 0 to limit do
        (assert (eq (trial-prime-p n) (not (null (pm:primep state n))))))))
  (pm:with-sieve (state 1000000)
    (pm:run-sieve state)
    (assert (= 78498 (pm:count-primes state)))))

(defun one-sample (seconds)
  (declare (type double-float seconds))
  (let* ((bytes (sb-ext:get-bytes-consed)) (gc sb-ext:*gc-run-time*)
         (start (get-internal-real-time))
         (deadline (+ start (ceiling (* seconds internal-time-units-per-second))))
         (finish start) (passes 0) (answer 0))
    (declare (type fixnum start deadline finish passes answer gc))
    (loop
      (pm:with-sieve (state 1000000)
        (pm:run-sieve state)
        (incf passes)
        (setf finish (get-internal-real-time))
        ;; Validate the last state before its native storage is released.
        (when (>= finish deadline)
          (setf answer (pm:count-primes state))
          (return))))
    (let ((allocated (- (sb-ext:get-bytes-consed) bytes))
          (gc-us (- sb-ext:*gc-run-time* gc)))
      (assert (= answer 78498))
      (let ((elapsed (/ (- finish start) (float internal-time-units-per-second 1d0))))
        (format t "~A;~D;~,9F;1;~A~%" (result-label) passes elapsed pm:*tags*)
        (format *error-output*
                "Valid: Pass; primes: ~D; Lisp heap bytes: ~D; GC time: ~D us~%"
                answer allocated gc-us)))))

(defun measure (&optional (seconds 5d0) (repeats 1))
  (check-type seconds (real 5))
  (check-type repeats (integer 1))
  (check)
  (dotimes (i repeats)
    ;; Compilation, validation and this full GC are outside the timed region.
    (sb-ext:gc :full t)
    (one-sample (float seconds 1d0))))
