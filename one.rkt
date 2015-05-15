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

(define coins '(1 5 10 25 50))

(define (nth xs idx)
  "Take the idx-th element of xs"
  (if (= idx 0) (first xs) (nth (rest xs) (- idx 1))))

















