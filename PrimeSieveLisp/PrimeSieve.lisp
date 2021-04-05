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
    (get-universal-time))
(loop 
    (if 
        (<= (- (get-universal-time) start) 10)
        (progn
            (prime-sieve 1000000)
            (run-sieve)
            (setq passes (+ passes 1))
        )
        (return 1)
    )
)

(defparameter duration 
    (- (get-universal-time) start))
(defparameter avg (/ duration passes))
(print (list "Passes:" passes "Time:" (- (get-universal-time) start) "Avg" avg "Count" (count-primes)))