#lang racket

(provide (all-defined-out))


(define (type-conversion-error obj t1 t2)
  (let ([t1 (send t1 get-type)]
        [t2 (send t2 get-type)])
    (send obj type-error (format "Cannot convert a ~a to ~a" t1 t2))))

(define (binary-error obj t1 t2)
  (let ([t1 (send t1 get-type)]
        [t2 (send t2 get-type)])
    (send obj type-error (format "Binary operation error ~a to ~a" t1 t2))))
