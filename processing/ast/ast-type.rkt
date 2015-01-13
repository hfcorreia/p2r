#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/bool
         "ast.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST types nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define type% 
  (class ast-node%
         (init-field to-type)

         (inherit read-error)

         (define/public (get-type) to-type)
         (define/public (promote-type type) (set! to-type type))

         (define/override (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

         (define/override (->type-check)
                          (read-error (format "Invalid use of ->type-check ~a" this)))


         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Primitive type
(define primitive-type% 
  (class type%
         (inherit-field to-type)


         (define/override (->racket)
                          (node->racket to-type))

         (define/override (->type-check) #t)

         (define/public (type=? from-type)
                        (symbol=? from-type to-type))

         (define/public (widening-conversion? from-type)
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

         (super-instantiate ())))

;;; Reference type
(define reference-type%
  (class type%
         (inherit-field to-type)

         (define/override (->racket)
                          (node->racket to-type))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define array-type% 
  (class type%
         (init-field dims)
         (inherit-field to-type)

         (define/override (->racket)
                          (node->racket to-type))

         (define/override (->type-check) #t)

         (super-instantiate ())))
