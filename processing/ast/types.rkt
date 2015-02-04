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

(define primitive-type% 
  (class type%
         (super-instantiate ())))

(define reference-type%
  (class type%
         (init-field qualified-types)
         (inherit-field type)

         (super-instantiate ())))

(define array-type% 
  (class reference-type%
         (init-field dims)
         (inherit-field type qualified-types)

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Structure:
;;;
;;; symbol-type = 'null   | 'string | 'boolean 
;;;             | 'char   | 'byte   | 'short 
;;;             | 'int    | 'long   | 'float  
;;;             | 'double | 'void   | 'color
;;;
;;; reference-type = 'null 
;;;                | 'string 
;;;                | reference-type%
;;;
;;; array-type = array-type% 
;;;
;;; type = symbol-type
;;;      | reference-type
;;;      | array-type
;;;      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reference-type?: symbol -> boolean
;; checks if the type is a reference type
(define (reference-type? type) 
  (is-a? reference-type% type))

;; array-type?: symbol -> boolean
;; checks if the type is an array type
(define (array-type? type)
  (is-a? array-type% type))

;; string-type?: symbol -> boolean
;; checks if the type is an string type
(define (string-type? type)
  (eq? type 'String))

;; color-type?: symbol -> boolean
;; checks if the type is an string type
(define (color-type? type)
  (eq? type 'color))

;; integral-type?: symbol -> boolean
;; checks if the type is an interger type
(define (integral-type? type)
  (memq type '(byte short int long char)))

;; numeric-type?: symbol -> boolean
;; checks if the type is an interger type
(define (numeric-type? type)
  (or (integral-type? type)
      (memq type '(float double))))

;; boolean-type?: symbol -> boolean
;; checks if the type is an interger type
(define (boolean-type? type)
  (eq? type 'boolean))

;; type=? : symbol symbol -> boolean
;; checks if two type symbols are the same
(define (type=? to-type from-type)
  (symbol=? to-type from-type))

;; widening-primitive-conversion? symbol symbol -> boolean
;; checks if from-type can be converted to to-type
(define (widening-primitive-conversion? to-type from-type)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; binary-check: predicate type type -> boolean
(define (binary-check predicate? t1 t2)
  (and (predicate? t1) (predicate? t2)))

;; binary-op-type-check?: operator type type -> type
;; checks if types are correct in binary operators
;; return the final type for the operation or 
;; an error symbol
(define (binary-operator-type op left right)
  (case op
    [(* / % *= /= %= -= -) 
     (if (binary-check numeric-type? left right)
       (binary-promotion left right)
       'error)]
    [(+ +=)
     (cond
       [(binary-check string-type? left right) 'string]
       [(binary-check numeric-type? left right) 
        (binary-promotion left right)]
       [else 'error])]
    [(< > <= >=)
     (if (binary-check numeric-type? left right)
       'boolean
       'error)]
    [(== !=)
     (cond 
       [(or (binary-check numeric-type? left right)
            (binary-check boolean-type? left right))
        'boolean]
     ; [(binary-check reference-or-array-type? left right)
     ;  (let ((right-to-left (castable? l r type-recs))
     ;        (left-to-right (castable? r l type-recs)))
     ;    (cond 
     ;      ((or right-to-left left-to-right) 'boolean)
     ;      (else (bin-op-equality-error 'both op l r src))))]
       [else 'error])]
    [(& ^ or &= ^= or=)
     (cond
       [(binary-check integral-type? left right) 
        (binary-promotion left right)]
       [(binary-check boolean-type? left right) 'boolean]
       [else 'error])]
    [(&& oror)      
     (if (binary-check boolean-type? left right) 
       'boolean
       'error)]))
           
;; binary-promotion: type type -> type
(define (binary-promotion t1 t2)
  (cond 
    [(or (eq? 'double t1) (eq? 'double t2))  'double]
    [(or (eq? 'float t1) (eq? 'float t2))  'float]
    [(or (eq? 'long t1) (eq? 'long t2))  'long]
    [else 'int]))
