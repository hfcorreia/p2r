#lang racket/base

(provide (all-defined-out))

(require racket/class
         "ast.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST types nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define type% 
  (class ast-node%
         (init-field type)

         (inherit read-error)

         (define/override (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Primitive type
(define primitive-type% 
  (class type%
         (inherit-field type)

         (define/override (->racket)
                          (send type ->racket))

         (super-instantiate ())))

;;; Reference type
(define reference-type%
  (class type%
         (inherit-field type)

         (define/override (->racket)
                          (send type ->racket))

         (super-instantiate ())))

(define array-type% 
  (class type%
         (init-field dims)
         (inherit-field type)

         (define/override (->racket)
                          (send type ->racket))

         (super-instantiate ())))
