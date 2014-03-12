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
   (error 
    (lambda (tok-ok? tok-name tok-value)
      (displayln (format "~a ~a ~a" tok-ok? tok-name tok-value))))
   (tokens operators literals seperators keywords empty-literals)
   (precs (left + -)
          (left * /))
   (grammar
    (stmts ((stmt)           $1)
           ((stmt stmts)     (make-stmts $1 $2))) 
    (stmt  ((expr semicolon) (make-stmt $1)))
    
    (expr  ((string-lit)         (make-num-exp $1))
           ((char-lit)           (make-num-exp $1))
           ((boolean-lit)        (make-num-exp $1))
           ((float-lit)          (make-num-exp $1))
           ((integer-lit)        (make-num-exp $1))
           ((double-lit)         (make-num-exp $1))
           ((expr + expr)    (make-arith-exp + $1 $3))
           ((expr - expr)    (make-arith-exp - $1 $3))
           ((expr * expr)    (make-arith-exp * $1 $3))
           ((expr / expr)    (make-arith-exp * $1 $3))))))