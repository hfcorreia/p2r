#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define type%
  (class object%
         (init-field type)

         (define/public (get-type) type)

         ;; reference-type?: -> boolean
         ;; checks if the type is a reference type
         (define/public (is-reference-type?) #f)

         ;; array-type?: -> boolean
         ;; checks if the type is an array type
         (define/public (is-array-type?) #f)

         ;; primitive-type?: -> boolean
         ;; checks if the type is an array type
         (define/public (is-primitive-type?) #f)

         ;; undef-type?:  -> boolean
         ;; checks if the type is an undef type
         (define/public (undef-type?)
                        (eq? type 'String))

         ;; string-type?:  -> boolean
         ;; checks if the type is an string type
         (define/public (string-type?)
                        (eq? type 'String))

         ;; color-type?:  -> boolean
         ;; checks if the type is an color type
         (define/public (color-type?)
                        (eq? type 'color))

         ;; char-type?:  -> boolean
         ;; checks if the type is an char type
         (define/public (char-type?)
                        (eq? type 'char))

         ;; integral-type?:  -> boolean
         ;; checks if the type is an interger type
         (define/public (integral-type?)
                        (memq type '(byte short int long char)))

         ;; long-or-int-type?:  -> boolean
         ;; checks if the type is an int or long
         (define/public (long-or-int-type?)
                        (memq type '(short int long)))

         ;; numeric-type?:  -> boolean
         ;; checks if the type is an numeric type
         (define/public (numeric-type?)
                        (or (integral-type?)
                            (memq type '(float double))))

         ;; boolean-type?:  -> boolean
         ;; checks if the type is an boolean type
         (define/public (boolean-type?)
                        (eq? type 'boolean))

         (super-instantiate ())))

(define primitive-type%
  (class type%

         ;; primitive-type?: -> boolean
         ;; checks if the type is an array type
         (define/override (is-primitive-type?) #t)

         (super-instantiate ())))

(define reference-type%
  (class type%
         (init-field qualified-types)
         (inherit-field type)

         ;; checks if the type is a reference type
         (define/override (is-reference-type?) #t)

         (super-instantiate ())))

(define array-type%
  (class reference-type%
         (init-field dims)
         (inherit-field type qualified-types)

         ;; array-type?: -> boolean
         ;; checks if the type is an array type
         (define/override (is-array-type?) #t)

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-type: symbol -> type%
;; Simplyfies the type creation
(define (create-type symbol)
   ;; need to add other types
    (make-object primitive-type% symbol))

;; create-types: (listof symbol) -> type%
;; Simplyfies of a list of types
(define (create-types types)
  (map create-type types))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Structure:
;;;
;;; symbol-type = 'null   | 'boolean
;;;             | 'char   | 'byte   | 'short
;;;             | 'int    | 'long   | 'float
;;;             | 'double | 'void   | 'color
;;;
;;; reference-type = 'null
;;;                | 'String
;;;                | reference-type%
;;;
;;; array-type = array-type%
;;;
;;; type = symbol-type
;;;      | reference-type
;;;      | array-type
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type=? : type% type% -> boolean
;; checks if two type symbols are the same
(define (type=? to-type from-type)
  (symbol=? (send to-type get-type)
            (send from-type get-type)))

;; object-type?: type% type% -> boolean
;; checks if from-type or to-type are of type object-type?
;; allowing the type-checker to skip the type-checking
(define (object-type? to-type from-type)
  (let ([to-type (send to-type get-type)]
        [from-type (send from-type get-type)])
    (or (symbol=? to-type 'Object) (symbol=? from-type 'Object))))

;; widening-primitive-conversion? type% type% -> boolean
;; checks if from-type can be converted to to-type
(define (widening-primitive-conversion? to-type from-type)
  (let ([to-type (send to-type get-type)]
        [from-type (send from-type get-type)])
    (cond
      [(symbol=? to-type 'char)
       (memq from-type '(int))]
      [(symbol=? to-type 'short)
       (memq from-type '(byte int))]
      [(symbol=? to-type 'int)
       (memq from-type '(byte short char))]
      [(symbol=? to-type 'long)
       (memq from-type '(byte short char int))]
      [(symbol=? to-type 'float)
       (memq from-type '(byte short char int long))]
      [(symbol=? to-type 'double)
       (memq from-type '(byte short char int long float))]
      [else #f])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; binary-check?: symbol type type -> boolean
(define (binary-check? predicate? t1 t2)
  (case predicate?
    ['numeric  (and (send t1 numeric-type?) (send t2 numeric-type?))]
    ['String   (or  (send t1 string-type?) (send  t2 string-type?))]
    ['boolean  (and (send t1 boolean-type?) (send t2 boolean-type?))]
    ['integral (and (send t1 integral-type?) (send t2 integral-type?))]))

;; binary-op-type-check?: operator type type -> (or/c type 'error)
;; checks if types are correct in binary operators
;; return the final type for the operation or
;; an error symbol
(define (binary-op-type-check op left right)
  (case op
    [(* / % *= /= %= -= -)
     (if (binary-check? 'numeric left right)
       (binary-promotion left right)
       'error)]
    [(+ +=)
     (cond
       [(binary-check? 'String left right) (create-type 'String)]
       [(binary-check? 'numeric left right)
        (binary-promotion left right)]
       [else 'error])]
    [(< > <= >=)
     (if (binary-check? 'numeric left right)
       (create-type 'boolean)
       'error)]
    [(== !=)
     (cond
       [(or (binary-check? 'numeric left right)
            (binary-check? 'boolean left right))
        (create-type 'boolean)]
       ; [(binary-check reference-or-array-type? left right)
       ;  (let ((right-to-left (castable? l r type-recs))
       ;        (left-to-right (castable? r l type-recs)))
       ;    (cond
       ;      ((or right-to-left left-to-right) 'boolean)
       ;      (else (bin-op-equality-error 'both op l r src))))]
       [else 'error])]
    [(<<  >> >>> <<= >>= >>>=)
     (if (binary-check? 'integral left right)
       (unary-promotion left)
       'error)]
    [(& ^ pipe &= ^= or=)
     (cond
       [(binary-check? 'integral left right)
        (binary-promotion left right)]
       [(binary-check? 'boolean left right) (create-type 'boolean)]
       [else 'error])]
    [(&& or)
     (if (binary-check? 'boolean left right)
       (create-type 'boolean)
       'error)]
    [else 'error]))

;; binary-promotion: type type -> type
(define (binary-promotion t1 t2)
  (let ([t1 (send t1 get-type)]
        [t2 (send t2 get-type)])
    (cond
      [(or (eq? 'double t1) (eq? 'double t2))  (create-type 'double)]
      [(or (eq? 'float t1) (eq? 'float t2))  (create-type 'float)]
      [(or (eq? 'long t1) (eq? 'long t2))  (create-type 'long)]
      [else (create-type 'int)])))

;; unary-check: op type -> (or/c type 'error)
(define (unary-op-type-check op t)
  (case op
    [(+ - pre++ pre-- pos++ pos--)
     (if (send t numeric-type?)
       (unary-promotion t)
       'error)]
    [(~)
     (if (send t integral-type?)
       (unary-promotion t)
       'error)]
    [(!)
     (if (send t boolean-type?)
       (create-type 'boolean)
       'error)]
    [else 'error]))

;; unary-promotion: type -> type
(define (unary-promotion t)
  (case t
    [(byte shor char) (create-type 'int)]
    [else t]))
