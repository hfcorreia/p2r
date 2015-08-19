#lang racket

(provide (all-defined-out))

(require (planet aml/rosetta)
         "runtime-bindings.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define/types (intersection [Object s1] [Object s2] -> Object)
  (intersection s1 s2))

(define/types (subtraction [Object s1] [Object s2] -> Object)
  (subtraction s1 s2))

(define/types (union [Object s1] [Object s2] -> Object)
  (union s1 s2))

(define/types (union [Object s1] [Object s2] [Object s3] -> Object)
  (union s1 s2 s3))

(define/types (spline [Object a] -> Object)
  (spline (vector->list a)))

(define/types (loft [Object a] [Object b] -> Object)
  (loft (list a b)))

(define/types (division [float min] [float max] [int n] -> Object)
  (list->vector (division min max n)))

(define/types (surface [Object a] -> Object)
  (surface-grid
   (map
    (lambda (x) (flatten (map vector->list (vector->list x))))
    (vector->list a))))

(define/types (thicken [Object a] [Object f] -> Object)
  (thicken a f))

(define/types (layer [String a] -> void)
  (layer a))
