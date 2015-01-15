#lang racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax background
  (syntax-rules ()
    [(_ rgb) 
     (error "background: Not Implemented")]
    [(_ rgb alpha) 
     (error "background: Not Implemented")]
    [(_ v1 v2 v3) 
     (error "background: Not Implemented")]
    [(_ v1 v2 v3 aplha) 
     (error "background: Not Implemented")]))

(define (noFill)
  (error "noFill: Not implemended"))

(define (stroke)
  (error "noFill: Not implemended"))
