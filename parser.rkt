#lang racket
(require parser-tools/yacc
         "lexer.rkt"
         "ast.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parser definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define simple-parser
  (parser
   (start stmts)
   (end EOF)
   (error void)
   (tokens operators literals seperators terminators)
   (precs (left + -)
          (left * /))
   (grammar
    (stmts ((stmt)           $1)
           ((stmt stmts)     (make-stmts $1 $2))) 
    (stmt  ((expr semicolon) (make-stmt $1)))
    
    (expr  ((integer)        (make-num-exp $1))
           ((expr + expr)    (make-arith-exp + $1 $3))
           ((expr - expr)    (make-arith-exp - $1 $3))
           ((expr * expr)    (make-arith-exp * $1 $3))
           ((expr / expr)    (make-arith-exp * $1 $3))))))