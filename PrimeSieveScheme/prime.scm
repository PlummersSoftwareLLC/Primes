(define correct-counts
  '((10 . 1)
    (100 . 25)
    (1000 . 168)
    (10000 . 1229)
    (100000 . 9592)
    (1000000 . 78498)
    (10000000 . 664579)
    (100000000 . 5761455)))


(define (count-primes bit-array)
  (do ((i 0 (+ i 1))
       (count 0))
      ((= i (vector-length bit-array)) count)
    (when (vector-ref bit-array i)
      (set! count (+ 1 count)))))
  

(define (validate-results bit-array size)
  (let loop ((correct-counts correct-counts))
    (cond
     ((null? correct-counts) #f)
     ((= (caar correct-counts) size) (= (count-primes bit-array)
                                        (cdar correct-counts)))
     (else (loop (cdr correct-counts))))))


(define (get-bit bit-array index)
  (if (even? index)
      #f
      (vector-ref bit-array (quotient index 2))))


(define (clear-bit bit-array index)
  (when (even? index) (display "You are setting even bits, which is sub-optimal"))
  (vector-set! bit-array (quotient index 2) #f))


(define (run-sieve sieve-size)
  (let ((bit-array (make-vector (quotient (+ sieve-size 1) 2) #t))
        (q (sqrt sieve-size)))
    
    (do ((factor 3 (+ factor 2)))
        ((>= factor q) bit-array)
      
      (do ((num factor (+ num 1)))
          ((or (> num sieve-size) (get-bit bit-array num)) (set! factor num)))
      
      (do ((num (* factor 3) (+ num (* factor 2))))
          ((> num sieve-size))
        (clear-bit bit-array num)))))
  

(define (print-results duration passes sieve-size bit-array)
  (display
   (format "Passes: ~a, Time: ~a, Avg: ~a, Limit: ~a, Count: ~a, Valid: ~a~%"
           passes
           (exact->inexact duration)
           (exact->inexact (/ duration passes))
           sieve-size
           (count-primes bit-array)
           (validate-results bit-array sieve-size))))


(define (precise-time t)
  (+ (time-second t) (/ (time-nanosecond t) 1000000000)))

(define (main size seconds)
  (do ((start (current-time time-monotonic))
       (now (current-time time-monotonic) (current-time time-monotonic))
       (bit-array (run-sieve size) (run-sieve size))
       (passes 0 (+ 1 passes)))
      ((>= (- (precise-time now) (precise-time start)) seconds)
       (print-results (- (precise-time now) (precise-time start))
                      passes
                      size
                      bit-array))))
       
(main 1000000 10)

