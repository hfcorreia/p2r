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
   (tokens operators literals seperators terminators)
   (precs (left + -)
          (left * /))
   (grammar
    (stmts ((stmt)           $1)
           ((stmt stmts)     (make-stmts $1 $2))) 
    (stmt  ((expr semicolon) (make-stmt $1)))
    
    (expr  ((string)         (make-num-exp $1))
           ((char)           (make-num-exp $1))
           ((boolean)        (make-num-exp $1))
           ((float)          (make-num-exp $1))
           ((integer)        (make-num-exp $1))
           ((double)         (make-num-exp $1))
           ((expr + expr)    (make-arith-exp + $1 $3))
           ((expr - expr)    (make-arith-exp - $1 $3))
           ((expr * expr)    (make-arith-exp * $1 $3))
           ((expr / expr)    (make-arith-exp * $1 $3))))))