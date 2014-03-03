#lang racket

(provide (all-defined-out))

(define-struct arith-exp (op e1 e2))
(define-struct num-exp (num))