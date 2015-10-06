#lang racket

(provide (all-defined-out))

(require "types.rkt")

(define (build-type-string t)
  (if (array-type? t)
    (string-append* (symbol->string (send (send t get-type) get-type))
                    (make-list (send t get-dims) "[]"))
    (send t get-type)))

(define (boolean-conversion-error obj t)
  (let ([t (build-type-string t)])
        (send obj type-error (format "Cannot convert a ~a to boolean" t))))

(define (type-conversion-error obj t1 t2)
  (let ([t1 (build-type-string t1)]
        [t2 (build-type-string t2)])
    (send obj type-error (format "Cannot convert a ~a to ~a" t1 t2))))

(define (binary-error obj op t1 t2)
  (let ([t1 (build-type-string t1)]
        [t2 (build-type-string t2)])
    (send obj
          type-error
          (format "The operator ~a is undefined for argument types(s) ~a,~a"
                  op t1 t2))))

(define (unary-error obj op t)
  (let ([t (build-type-string t)])
    (send obj
          type-error
          (format "The operator ~a is undefined for argument types(s) ~a"
                  op t))))

(define (array-type-error obj id)
  (send obj read-error  (format "~a is not of type array." id)))

(define (binding-not-found obj id)
  (send obj read-error (format "Cannot find anything named \"~a\"" id)))

(define (method-not-applicable obj id types)
  (send obj read-error (format "The method ~a is not applicable for arguments ~a" id types)))

(define (return-error obj)
  (send obj read-error (format "Unreachable code")))

(define (duplicate-variable obj id)
  (send obj read-error (format "Duplicate local variable ~a" id)))

(define (duplicate-method obj id args)
  (send obj read-error (format "Duplicate method ~a~a" id args)))
