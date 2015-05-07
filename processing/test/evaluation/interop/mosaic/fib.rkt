#lang racket

(provide fib)

(define (fib n)
  (if (or (equal? n 1) (equal? n 2))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

