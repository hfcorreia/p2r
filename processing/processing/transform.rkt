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
;;;
#|
(define-syntax applyMatrix
  (syntax-rules ()
    [(_ src)
     (error "applyMatrix: not implemented")]
    [(_ src
        n00 n01 n02
        n10 n11 n12)
     (error "applyMatrix: not implemented")]
    [(_ src
        n00 n01 n02 n03
        n10 n11 n12 n13
        n20 n21 n22 n23
        n30 n31 n32 n33)
     (error "applyMatrix: not implemented")]))

(define (popMatrix)
  (error "popMatrix: not implemented"))

(define (printMatrix)
  (error "printMatrix: not implemented"))

(define (pushMatrix)
  (error "pushMatrix: not implemented"))

(define (resetMatrix)
  (error "resetMatrix: not implemented"))

(define-syntax rotate
  (syntax-rules ()
    [(_ src)
     (error "rotate: not implemented")]
    [(_ ang x y z)
     (error "rotate: not implemented")]))

(define (rotateX ang)
  (error "rotateX: not implemented"))

(define (rotateY ang)
  (error "rotateY: not implemented"))

(define (rotateZ ang)
  (error "rotateZ: not implemented"))

(define-syntax scale
  (syntax-rules ()
    [(_ src)
     (error "scale: not implemented")]
    [(_ x y)
     (error "scale: not implemented")]
    [(_ ang x y z)
     (error "scale: not implemented")]))

(define (shearX ang)
  (error "shearX: not implemented"))

(define (shearY ang)
  (error "shearY: not implemented"))

(define-syntax translate
  (syntax-rules ()
    [(_ x y)
     (error "translate: not implemented")]
    [(_ ang x y z)
     (error "translate: not implemented")]))|#

