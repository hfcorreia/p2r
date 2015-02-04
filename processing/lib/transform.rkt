#lang racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     (error "translate: not implemented")]))

