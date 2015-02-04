#lang racket

(provide compile-processing
         compile-processing-repl
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

;;; compile-processing : ast -> (listof syntax-object?)
;;; generates the list of syntax-objects based on the ast
(define (compile-processing ast)
  (node->bindings ast (make-object global-scope%))
  (node->type-check ast)
  (node->racket ast))

;;; compile-processing-repl : ast -> (listof syntax-object?)
;;; generates the list of syntax-objects based on the ast consumed by the repl
(define (compile-processing-repl ast)
  (send ast ->repl))
