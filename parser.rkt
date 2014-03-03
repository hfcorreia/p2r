#lang racket
(require parser-tools/yacc
         "lexer.rkt"
         "ast.rkt")

(define simple-parser
  (parser
   (start expr)
   (end EOF)
   (error void)
   (tokens operators literals terminator)
   (precs (left + -)
          (left * /))
   (grammar
    (expr ((integer)      $1)
         ((expr + expr)   (printer + $1 $3))
         ((expr - expr)   (printer - $1 $3))
         ((expr * expr)   (printer * $1 $3))
         ((expr / expr)   (printer / $1 $3))))))
         
(define-syntax-rule 
  (printer op ...)
  (begin
    (displayln (list op ...))
    (eval (op ...))))