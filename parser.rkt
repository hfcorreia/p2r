#lang racket
(require parser-tools/yacc
         "lexer.rkt"
         "ast.rkt")

(define simple-parser
  (parser
   (start expr)
   (end EOF)
   (error void)
   (tokens operators literals terminators)
   (precs (left + -)
          (left * /))
   (grammar
    (expr ((integer)      (make-num-exp $1))
          ((expr + expr)   (make-arith-exp + $1 $3))
          ((expr - expr)   (make-arith-exp - $1 $3))
          ((expr * expr)   (make-arith-exp * $1 $3))
          ((expr / expr)   (make-arith-exp * $1 $3))))))