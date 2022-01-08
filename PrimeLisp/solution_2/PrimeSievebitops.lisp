;;;; loosely based on sieve_1of2.c by  by Daniel Spangberg
;;;
;;; set-bits-dense based on https://github.com/PlummersSoftwareLLC/Primes/pull/680
;;;
;;; run as:
;;;     sbcl --script PrimeSievebitops.lisp
;;;


(in-package "SB-X86-64-ASM")

#+(and :sbcl :x86-64)
(eval-when (:load-toplevel :compile-toplevel :execute)

(when (member (lisp-implementation-version) '("2.0.0" "2.1.8" "2.2.0") :test #'equalp)
(progn

;;; "OR r, imm1" + "OR r, imm2" -> "OR r, (imm1 | imm2)"
;;;
;;; This pattern combines successive OR instructions
;;; with immediate operands and the same target register.
;;; It handles not only immediates up to 32 bit
;;; but RIP relative constants as well.
;;; It has two flaws: the value of the RIP relative constants
;;; are found by linear search (slow!), and more importantly:
;;; no longer used constants are NOT removed from the constant pool.
;;; So if lots of OR instructions are combined then we end up
;;; with lots of unused constants.
(defpattern "or + or -> or" ((or) (or)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next)))
    (labels ((larger-of (size1 size2)
               (if (or (eq size1 :qword) (eq size2 :qword)) :qword :dword))

             (find-constant-value (c)
               (let ((constants (asmstream-constant-vector *asmstream*)))
                 (when (plusp (length constants))
                   (dovector (constant constants t)
                     (when (eq c (cdr constant))
                       (return (cdar constant)))))))

             (value-of-constant (op)
               (cond ((ea-p op) (find-constant-value (ea-disp op)))
                     (t op))))

      (when (and (gpr-tn-p dst1)
                 (location= dst2 dst1)
                 (member size1 '(:qword :dword))
                 (or (typep src1 '(signed-byte 32))
                     (ea-p src1))
                 (member size2 '(:dword :qword))
                 (or (typep src2 '(signed-byte 32))
                     (ea-p src2)))
        (let ((src11 (value-of-constant src1))
              (src22 (value-of-constant src2)))
          (setf (stmt-operands next)
                (if (equalp "2.0.0" (lisp-implementation-version))
                      `(,(larger-of size1 size2) ,dst2 ,(sb-vm::constantize (logior src11 src22)))  ; 2.0.0
                  `(,(encode-size-prefix (larger-of size1 size2)) ,dst2 ,(sb-vm::constantize (logior src11 src22)))))  ; 2.1.8
          (add-stmt-labels next (stmt-labels stmt))
          (delete-stmt stmt)
          next)))))

;;; "OR r, imm1" + "OR r, imm2" -> "OR r, (imm1 | imm2)"
;;; OR sets the flags according to the result value so combining
;;; several OR instructions should be ok.
(defpattern "or + or -> or (signed-byte 32)" ((or) (or)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next)))
    (flet ((larger-of (size1 size2)
             (if (or (eq size1 :qword) (eq size2 :qword)) :qword :dword)))
      (when (and (gpr-tn-p dst1)
                 (location= dst2 dst1)
                 (member size1 '(:qword :dword))
                 (typep src1 '(signed-byte 32))
                 (member size2 '(:dword :qword))
                 (typep src2 '(signed-byte 32)))
        (setf (stmt-operands next)
              (if (equalp "2.0.0" (lisp-implementation-version))
                    `(,(larger-of size1 size2) ,dst2 ,(logior src1 src2))  ; 2.0.0
                `(,(encode-size-prefix (larger-of size1 size2)) ,dst2 ,(logior src1 src2))))  ; 2.1.8
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

) ; end progn
) ; end when
) ; end eval-when

(in-package "CL-USER")


(declaim
  (optimize (speed 3) (safety 0) (debug 0) (space 0))

  (inline nth-bit-set-p)
  (inline set-nth-bit)

  (inline set-bits-simple)
  ;(inline set-bits-unrolled)  ; don't inline this dozens of times or set-bits-dense will get too big
  (inline set-bits-dense))


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

(deftype nonneg-fixnum ()
  `(integer 0 ,most-positive-fixnum))
  ;`(unsigned-byte ,+bits-per-word+))

(deftype sieve-element-type ()
  `(unsigned-byte ,+bits-per-word+))

(deftype sieve-array-type ()
  `(simple-array sieve-element-type 1))


(defclass sieve-state ()
  ((maxints :initarg :maxints
            :type nonneg-fixnum
            :accessor sieve-state-maxints)

   (a       :initarg :a
            :type sieve-array-type
            :accessor sieve-state-a)))


(defun create-sieve (maxints)
  (declare (nonneg-fixnum maxints))
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


(defun set-nth-bit (a n)
  "Set n-th bit in array a to 1."
  (declare (type sieve-array-type a)
           (type nonneg-fixnum n))
  (multiple-value-bind (q r) (floor n +bits-per-word+)
    (declare (nonneg-fixnum q r))
    (setf #1=(aref a q)
             (logior #1# (ash 1 r))))
  0)


(defun set-bits-simple (bits first-incl last-excl every-nth)
  "Set every every-nth bit in array bits between first-incl and last-excl."
  (declare (type nonneg-fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))
  (loop while (< first-incl last-excl)
        do (set-nth-bit bits first-incl)
           (incf first-incl every-nth)))


(defun set-bits-unrolled (bits first-incl last-excl every-nth)
  "Set every every-nth bit in array bits between first-incl and last-excl."
  (declare (type nonneg-fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))

  ; use an unrolled loop to set every every-th bit to 1
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

(defun generate-set-bits-modulo (startbit n)
  "Generate statements to set every nth bit in n words, starting at startbit.
The generated code contains references to the variable 'startword'."
  (loop for word
        from 0
        below n
        append (multiple-value-bind (wordoffset bitoffset) (floor startbit +bits-per-word+)
                 (if (>= (+ bitoffset n) +bits-per-word+)

                       ; only 1 bit to set in the current word - don't use tmp variable
                       (prog1 `((setf #1=(aref bits (+ startword ,(+ word wordoffset)))
                                      (logior #1# #2=,(ash 1 bitoffset))))
                              (incf startbit n)
                              (decf startbit +bits-per-word+))

                   ; more than 1 bit to set - use tmp variable
                   `((let ((tmp (logior #1# #2#)))
                       (declare (type sieve-element-type tmp))

                       ,@(loop for j from (+ startbit n) by n
                               for i from (+ bitoffset n) by n
                               while (< (+ i n) +bits-per-word+)
                               collect `(setq tmp (logior tmp #3=,(ash 1 i))) into ret
                               finally (setq startbit j)
                                       (incf startbit n)
                                       (decf startbit +bits-per-word+)
                                       (return (append ret `((setf #1# (logior tmp #3#))))))))))))


(defun generate-dense-loop (first n)
  "Generate a loop statement to set every nth bit, starting at first.
The generated code contains references to the variable 'last-excl'."
  (let ((bits-per-step (* n +bits-per-word+)))
    `((let ((startword 0))
        ,@(generate-set-bits-modulo first n))

      (loop for bit of-type nonneg-fixnum
            from ,bits-per-step
            below (- last-excl ,bits-per-step)
            by ,bits-per-step
            do (let ((startword (floor bit +bits-per-word+)))
                 ,@(generate-set-bits-modulo (mod (+ first bits-per-step) n) n))
            finally (set-bits-unrolled bits (+ bit ,(mod (+ first bits-per-step) n)) last-excl ,n)))))

) ; end eval-when


(defmacro generate-cond-stmt ()
  "Expand into a cond stmt whose branches all set every 'every-nth' bit in the array 'bits'.
The generated code contains references to the variables 'bits', 'first-incl', 'last-excl' and 'every-nth'.
Branches for low values of 'every-nth' (up to 53) will set bits using unrolled dense loops,
fallback for higher values is calling 'set-bits-unrolled'."
  `(cond ,@(loop for x from 3 to 53 by 2
                 collect `((= every-nth ,x)
                           ,@(generate-dense-loop (floor (expt x 2) 2) x)))
         (t (set-bits-unrolled bits first-incl last-excl every-nth))))


(defun set-bits-dense (bits first-incl last-excl every-nth)
  "Set every every-nth bit in array bits between first-incl and last-excl."
  (declare (type nonneg-fixnum first-incl last-excl every-nth)
           (type sieve-array-type bits))
  (if (< every-nth (floor last-excl +bits-per-word+))
        (generate-cond-stmt)
    (set-bits-unrolled bits first-incl last-excl every-nth)))


(defun run-sieve (sieve-state)
  (declare (type sieve-state sieve-state))

  (let* ((rawbits (sieve-state-a sieve-state))
         (sieve-size (sieve-state-maxints sieve-state))
         (sieve-sizeh (ceiling sieve-size 2))
         (factor 0)
         (factorh 1)
         (qh (ceiling (isqrt sieve-size) 2)))
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

      (set-bits-dense rawbits (floor (the nonneg-fixnum (* factor factor)) 2) sieve-sizeh factor))))


(defun count-primes (sieve-state)
  (declare (sieve-state sieve-state))
  (let ((max (sieve-state-maxints sieve-state))
        (bits (sieve-state-a sieve-state))
        (result 0))
    (declare (nonneg-fixnum result))
    (loop for i of-type nonneg-fixnum
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



(let* ((ignored (sleep 5)) ; sleep for 5 seconds to let CPU cool down
       (passes 0)
       (start (get-internal-real-time))
       (end (+ start (* internal-time-units-per-second 5)))
       result)
  (declare (nonneg-fixnum passes))

  (loop while (<= (get-internal-real-time) end)
        do (setq result (run-sieve (create-sieve 1000000)))
           (incf passes))

  (let* ((duration  (/ (- (get-internal-real-time) start) internal-time-units-per-second))
         (avg (/ duration passes)))
    (when *list-to* (list-primes result))
    (format *error-output* "Algorithm: base w/ dense bitops  Passes: ~d  Time: ~f Avg: ~f ms Count: ~d  Valid: ~A~%"
            passes duration (* 1000 avg) (count-primes result) (validate result))

    (format t "mayerrobert-cl-dense;~d;~f;1;algorithm=base,faithful=yes,bits=1~%" passes duration)))


; uncomment the following line to display the generated loop stmt for setting every 3rd bit starting at 4
;(format *error-output* "The bit-setting loop for startmod=84 and skipmod=13:~%~A~%" (generate-dense-loop 84 13))


; uncomment the following line to display the generated cond stmt containing dense bit-setting loops for the first few distances
;(format *error-output* "Expansion of macro generate-cond-stmt:~%~A~%" (macroexpand-1 '(generate-cond-stmt)))
