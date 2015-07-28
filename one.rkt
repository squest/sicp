#lang racket

(define (sum-sieve lim)
  (let ((refs (make-vector (+ lim 1) true))
        (llim (sqrt lim)))
    (define (loopj j step)
      (if (<= j lim)
          (begin (vector-set! refs j false)
                 (loopj (+ j step) step))
          false))
    (define (loop i res)
      (if (> i lim)
          res 
          (if (vector-ref refs i)
              (if (<= i llim)
                  (begin (loopj (* i i) (+ i i))
                         (loop (+ i 2) (+ i res)))
                  (loop (+ i 2) (+ i res)))
              (loop (+ i 2) res))))
    (loop 3 2)))

(define (sum-sieve2 lim)
  (let* ((refs (make-vector (+ lim 1) true))
         (llim (integer-sqrt lim))
         (hlim (if (even? llim) (+ llim 1) (+ llim 2))))
    (define (outer i res)
      (define (inner j)
        (when (<= j lim)
          (vector-set! refs j false)
          (inner (+ j i i))))
      (if (<= i llim)
          (if (vector-ref refs i)
              (begin (inner (* i i))
                     (outer (+ i 2) (+ i res)))
              (outer (+ i 2) res))
          res))
    (define (finder i res)
      (if (> i lim)
          res
          (if (vector-ref refs i)
              (finder (+ i 2) (+ i res))
              (finder (+ i 2) res))))
    (finder hlim (outer 3 2))))


(define (my-sqrt n)
  (define (improve-guess guess)
    (/ (+ guess (/ n guess)) 2.0))
  (define (good-enough? x)
    (< (abs (- (* x x) n)) 0.0001))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve-guess guess))))
  (sqrt-iter 1))

;; linear recursions

(define (factorial i (res 1))
  (if (= i 0) res (factorial (- i 1) (* res i))))

(define (factorial-2 i)
  (define (fact-iter j res)
    (if (= j i) (* res j) (fact-iter (+ j 1) (* res j))))
  (fact-iter 1 1))

;; tree recursions

(define (fibo i)
  (define (fibo-iter i a b)
    (if (= i 0) a (fibo-iter (- i 1) (+ a b) a)))
  (fibo-iter i 1 0))

(define (fibo-1 i (a 1) (b 0))
  (if (= i 0) a (fibo-1 (- i 1) (+ a b) a)))

(define (fibo-list i)
  (define (fibo-iter i a b res)
    (if (= i 0) 
        res
        (fibo-iter (- i 1) (+ a b) a (append res (list a)))))
  (fibo-iter i 1 0 '()))

(define (fibo-list-1 i (a 1) (b 0) (res '()))
  (if (= i 0)
      (reverse res)
      (fibo-list-1 (- i 1) (+ a b) a (cons a res))))

(define (fibo-list-2 i)
  (define (fibo-iter i a b res)
    (if (= i 0) (reverse res) (fibo-iter (- i 1) (+ a b) a (cons a res))))
  (fibo-iter i 1 0 '()))

;; coins summation

(define coins '(1 2 5 10 20 50 100 200))

(define (nth xs idx)
  "Take the idx-th element of xs"
  (if (= idx 0) (first xs) (nth (rest xs) (- idx 1))))

(define (sum-coins amount (coin 7))
  (match* (amount coin)
    [(0 _) 1]
    [((? negative?) _) 0]
    [(_ 0) 1]
    [(amount coin) 
     (let ((coin-value (nth coins coin)))
       (foldl + 0 (map (lambda (x)
                         (sum-coins 
                          (- amount (* coin-value x))
                          (- coin 1)))
                       (range (+ 1 (quotient amount coin-value))))))]))

(define/match (faktorial n (res 1))
  ((0 _) res)
  ((1 _) res)
  ((n res) (faktorial (- n 1) (* res n))))

(define/match (coin-sums amount (coin 7))
  [(0 _) 1]
  [((? negative?) _) 0]
  [(_ 0) 1]
  [(amount coin)
   (let [(cvalue (nth coins coin))]
     (foldl + 0 (map (lambda (x)
                       (coin-sums 
                        (- amount (* cvalue x))
                        (- coin 1)))
                     (range (+ 1 (quotient amount cvalue))))))])

(define/match (fibo-pat i (a 1) (b 0))
  [(1 _ _) a]
  [(i a b) (fibo-pat (- i 1) (+ a b) a)])

(define/match (fibolist-pat i (a 2) (b 1) (res '(1)))
  [(1 _ _ res) (reverse res)]
  [(i a b res) (fibolist-pat (- i 1) (+ a b) a (cons a res))])

(define/match (fast-expt a m)
  [(_ 0) 1]
  [(a 1) a]
  [(a m) (let [(half (fast-expt a (quotient m 2)))]
           (if (even? m)
               (* half half)
               (* a half half)))])

(define/match (expt-1 a m (res 1))
  [(_ 0 res) res]
  [(a m res) (expt-1 a (- m 1) (* a res))])

(define/match (gcd-1 a b)
  [(0 b) b]
  [(a 0) a]
  [(1 b) 1]
  [(a 1) 1]
  [(a b) (if (> a b) 
             (gcd-1 (remainder a b) b) 
             (gcd-1 (remainder b a) a))])

(define (split xs)
  (let [(count (length xs))]
    (list (take xs count) (drop xs count))))

(define (divisors n (i 2) (res (list 1 n)))
  (cond 
    [(> (* i i) n) (sort res <)]
    [(= (* i i) n) (sort (cons i res) <)]
    [(= 0 (remainder n i)) 
     (divisors n (+ i 1) (cons i (cons (quotient n i) res)))]
    [else (divisors n (+ i 1) res)]))

(define/match (prime? p (i 3))
  [((? (lambda (x) (< x 2))) _) false]
  [(2 _) true]
  [((? even?) _) false]
  [(p i) (cond 
           [(> (* i i) p) true]
           [(= 0 (remainder p i)) false]
           [else (prime? p (+ i 2))])])

(define (odd-prime? p (i 3))
  (cond 
    [(> (* i i) p) true]
    [(= 0 (remainder p i)) false]
    [else (odd-prime? p (+ i 2))]))

(define/match (modex a m modi)
  [(_ 0 _) 1]
  [(_ 1 _) (remainder a modi)]
  [(a m modi) 
   (let [(half (modex a (quotient m 2) modi))]
     (if (even? m) 
         (remainder (* half half) modi)
         (remainder (* a half half) modi)))])

(define/match (reduce f xs (acc null))
  [(_ (? empty?) _) acc]
  [(_ (list-rest a b) (? null?)) (reduce f b a)]
  [(_ (list-rest a b) acc) (reduce f b (f acc a))])






















































