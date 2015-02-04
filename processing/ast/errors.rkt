#lang racket

(provide (all-defined-out))


(define (type-conversion-error obj t1 t2)
  (send obj type-error (format "Cannot convert a ~a to ~a" t1 t2)))
