#lang racket

(provide compile-processing
         compile-processing-repl
         type-check
         bindings-check
         build-ast)

(require "ast/ast.rkt"
         "parser.rkt"
         "ast/bindings.rkt")

;;; build-ast: file input-port= #f -> (listof ast-node%)
;;; parses the input file and constructs an ast of ast-node%
(define (build-ast file #:input-port [input-port #f])
  (if (eq? input-port #f)
    (with-input-from-file
      file
      (lambda () (parse-processing file (current-input-port)))
      #:mode 'text)
    (parse-processing file input-port)))

;;; type-check : ast -> (or/c #t type-error)
;;; traverse the ast checking if types are correct
(define (type-check ast)
  (node->type-check ast))

;;; bindings-check : ast -> (or/c #t type-error)
;;; traverse the ast and create the necessary scopes
(define (bindings-check ast)
  (node->bindings ast (make-object global-scope%)))

;;; compile-processing : ast -> (listof syntax-object?)
;;; generates the list of syntax-objects based on the ast
(define (compile-processing ast)
  (node->racket ast))

;;; compile-processing-repl : ast -> (listof syntax-object?)
;;; generates the list of syntax-objects based on the ast consumed by the repl
(define (compile-processing-repl ast)
  (send ast ->repl))
