#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro that provides and defines a new struct for the AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax p-define-struct
  (syntax-rules ()
    [(_ (name inherit) fields)
     (begin
       (provide (struct-out name))
       (define-struct (name inherit) fields #:mutable #:transparent))]
    [(_ name fields)
     (begin
       (provide (struct-out name))
       (define-struct name fields #:mutable #:transparent))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST struct definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; (make-arith-exp (operator exp1 exp2))
(p-define-struct arith-exp (operator exp1 exp2))

;;; (make-num-exp (number))
(p-define-struct num-exp (number))

;;; (make-stmts (stmt stmts))
(p-define-struct stmts (stmt stmts))

;;; (make-stmt (stmt))
(p-define-struct stmt (stmt))