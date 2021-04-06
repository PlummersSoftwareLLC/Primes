(define correct-counts
  '((10 . 1)
    (100 . 25)
    (1000 . 168)
    (10000 . 1229)
    (100000 . 9592)
    (1000000 . 78498)
    (10000000 . 664579)
    (100000000 . 5761455)))


(define (find-first vector start stop)
  (let finder ((i start))
    (cond
     ((> i stop) start)
     ((even? i) (finder (+ 1 i)))
     ((vector-ref vector (quotient i 2)) i)
     (else (finder (+ 1 i))))))


(define (update vector start stop)
  (do ((i (* start 3) (+ i (* start 2))))
      ((> i stop))
    (when (even? i) (display "You are setting even bits, which is sub-optimal"))
    (vector-set! vector (quotient i 2) #f)))


(define (prime-sieve size)
  (let ((bits (make-vector (quotient (+ size 1) 2) #t))
        (q (sqrt size)))
    (let run-sieve ((factor 3))
      (if (>= factor q)
          bits
          (let ((new-factor (find-first bits factor size)))
            (update bits new-factor size)
            (run-sieve (+ new-factor 2)))))))


(define (bit-count vector)
  (let loop ((count 0)
             (i 0))
    (cond
     ((= i (vector-length vector)) count)
     ((vector-ref vector i) (loop (+ 1 count) (+ 1 i)))
     (else (loop count (+ 1 i))))))


(define (validate-results result size)
  (let loop ((correct-counts correct-counts))
    (cond
     ((null? correct-counts) #f)
     ((= (caar correct-counts) size) (= (bit-count result)
                                        (cdar correct-counts)))
     (else (loop (cdr correct-counts))))))


(define (precise-time t)
  (+ (time-second t) (/ (time-nanosecond t) 1000000000)))

(define (run-timer size seconds)
  (do ((start (current-time time-monotonic))
       (curr (current-time time-monotonic) (current-time time-monotonic))
       (bit-result (prime-sieve size) (prime-sieve size))
       (count 0 (+ 1 count)))
      ((>= (- (precise-time curr) (precise-time start)) seconds)
       (display
        (format "Passes: ~a, Time: ~a, Avg: ~a, Limit: ~a, Count: ~a, Valid: ~a~%"
                count
                (exact->inexact (- (precise-time curr) (precise-time start)))
                (exact->inexact (/ (- (precise-time curr) (precise-time start)) count))
                size
                (bit-count bit-result)
                (validate-results bit-result size))))))

(run-timer 1000000 10)

