(defparameter rawbits ())
(defparameter sieveSize 0)
(defparameter num 0)
(defparameter q 0)
(defparameter factor 0)

(defun prime-sieve (limit)
    (setq sieveSize  limit)
    (setq rawbits  
        (make-array 
            (floor 
                (/ 
                    (+ limit 1) 2))
            :element-type 'bit)
    )
    (setq rawbits 
        (bit-eqv rawbits rawbits))
)

(defun get-bit (index)
    (if 
        (eq 
            (mod index 2) 0) 0 
        (aref rawbits 
            (floor 
                (/ index 2))))
)

(defun clear-bit (index)
    (setf 
        (aref rawbits 
            (floor 
                (/ index 2))) 0)
)

(defun run-sieve ()
    (setq factor 3) 
    (setq q 
        (sqrt sieveSize))
    (loop 
        (if 
            (>= factor q) 
            (return 1))
        (block continueExit
            (loop for num from factor to sieveSize  do
                (if 
                    (eq 
                        (get-bit num) 1)
                    (progn
                        (setq factor num)
                        (return-from continueExit 1)
                    )
                )
            )
        )
        (loop for num from 
            (* factor 3) to sieveSize by 
            (* factor 2) do 
            (clear-bit num))
        (setq factor 
            (+ factor 2))
    )
)

(defun count-primes ()
    (reduce #'+ rawbits)
)

(defparameter passes 0)
(defparameter start 
    (get-internal-real-time))
(defparameter runtime 
    (* internal-time-units-per-second 10))

(loop 
    (if 
        (<= (- (get-internal-real-time) start) runtime)
        (progn
            (prime-sieve 1000000)
            (run-sieve)
            (setq passes (+ passes 1))
        )
        (return 1)
    )
)

(defparameter duration 
    (/ (- (get-internal-real-time) start) internal-time-units-per-second))
(defparameter avg (/ duration passes))
(print (list "Passes:" passes "Time:" duration "Avg" avg "Count" (count-primes)))

;; Following 2 lines added by rbergen to conform to drag race output format
(terpri)
(format t "~%mikehw;~d;~f;1;algorithm=base,faithful=no,bits=1~%" passes duration)
