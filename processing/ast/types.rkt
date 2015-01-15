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
         (init-field to-type)

         (define/public (get-type) to-type)
         (define/public (promote-type type) (set! to-type type))

         (define/public (type=? from-type)
                        (symbol=? from-type to-type))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Primitive type
(define primitive-type% 
  (class type%
         (inherit-field to-type)

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

         (define/public (widening-conversion? from-type) #f)

         (super-instantiate ())))

(define array-type% 
  (class type%
         (init-field dims)
         (inherit-field to-type)

         (super-instantiate ())))
