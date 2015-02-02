#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/bool
         "ast.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define type% 
  (class object%
         (init-field type)

         (define/public (get-type) type)

         (super-instantiate ())))

;;; Primitive type
(define primitive-type% 
  (class type%
         (inherit-field type)

         (super-instantiate ())))

;;; Reference type
(define reference-type%
  (class type%
         (inherit-field type)

         (super-instantiate ())))

(define array-type% 
  (class type%
         (init-field dims)
         (inherit-field type)

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (type=? to-type from-type)
  (symbol=? to-type from-type))

(define (widening-conversion? to-type from-type)
  (cond
    [(symbol=? to-type 'char)     #f]
    [(symbol=? to-type 'short) 
     (memq from-type '(byte int))]
    [(symbol=? to-type 'int)
     (memq from-type '(byte short char))]
    [(symbol=? to-type 'long)
     (memq from-type '(byte short char int))]
    [(symbol=? to-type 'float)
     (memq from-type '(byte short char int long))]
    [(symbol=? to-type 'double)
     (memq from-type '(byte short char int long float))]))
