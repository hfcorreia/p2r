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

         (define/override (->type-check)
                          (read-error (format "Invalid use of ->type-check ~a" this)))

         (define/public (type-error from-type)
                        (read-error (format "Cannot convert from ~a to ~a"
                                            from-type type)))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Primitive type
(define primitive-type% 
  (class type%
         (inherit-field type)

         (define/override (->racket)
                          (node->racket type))

         (define/override (->type-check) #t)

         (inherit type-error)
         (define/public (check-literal-type? literal-type)
                          (cond 
                            [(eq? literal-type type) type]
                            [else (type-error literal-type)]))

         (super-instantiate ())))

;;; Reference type
(define reference-type%
  (class type%
         (inherit-field type)

         (define/override (->racket)
                          (node->racket type))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define array-type% 
  (class type%
         (init-field dims)
         (inherit-field type)

         (define/override (->racket)
                          (node->racket type))

         (define/override (->type-check) #t)

         (super-instantiate ())))
