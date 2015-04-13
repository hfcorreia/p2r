#lang racket

(provide (all-defined-out))

(require "ast/ast.rkt"
         "parser.rkt")

;;; build-ast: file input-port= #f -> (listof ast-node%)
;;; parses the input file and constructs an ast of ast-node%
(define (build-ast file [input-port #f])
  (if (eq? input-port #f)
    (with-input-from-file
      file
      (lambda () (parse-processing file (current-input-port)))
      #:mode 'text)
    (parse-processing file input-port)))

;;; build-repl-interaction: input-port -> (listof ast-node%)
;;; parses the input-port and constructs an ast of ast-node%
(define (build-repl-interaction src input-port)
  (define (filter-unready-port in)
    (let loop ([chars (list)])
      (if (and (char-ready? in)
               (not (eof-object? (peek-char in))))
        (loop (cons (read-char in) chars))
        (open-input-string
          (apply string (reverse chars))))))
  (parse-processing-repl src (filter-unready-port input-port)))

;;; type-check : ast -> (or/c #t type-error)
;;; traverse the ast checking if types are correct
(define (type-check ast)
  (node->type-check ast))

;;; bindings-check : ast -> (or/c #t type-error)
;;; traverse the ast and create the necessary scopes
(define (bindings-check ast scope)
  (node->bindings ast scope))

;;; compile-processing : ast -> (listof syntax-object?)
;;; generates the list of syntax-objects based on the ast
(define (compile-processing ast scope)
  ;; (pretty-display (node->print ast))
  ;; (pretty-display (send scope get-scope))
  (bindings-check ast scope)
  (type-check ast)
  (node->racket ast))

(define (compile-processing-repl ast scope)
  ;; (pretty-display (node->print ast))
  (bindings-check ast scope)
  (type-check ast)
  (node->racket ast))
