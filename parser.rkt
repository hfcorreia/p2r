(module processing-parser racket
  (require parser-tools/yacc
           "lexer.rkt"
           "ast.rkt")
  
  (provide parse-processing)
  
  (define (parse-processing src port)
      (port-count-lines! port)
      (processing-parser (lambda () (lex port))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Parser definition
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define processing-parser
    (parser
     (src-pos)
     (start stmts)
     (end EOF)
     (error 
      (lambda (tok-ok? tok-name tok-value start-pos end-pos)
        (displayln (format "~a ~a ~a" tok-ok? tok-name tok-value))))
     (tokens operators literals seperators keywords empty-literals)
     (precs (left + -)
            (left * /))
     
     (grammar
      (stmts
       ((stmt)           $1)
       ((stmt stmts)     (make-stmts $1 $2))) 
      
      (stmt
       ((expr semicolon) (make-stmt $1)))
      
      (expr  
       ((literals)       (make-num-exp $1))
       ((expr + expr)    (make-arith-exp + $1 $3))
       ((expr - expr)    (make-arith-exp - $1 $3))
       ((expr * expr)    (make-arith-exp * $1 $3))
       ((expr / expr)    (make-arith-exp * $1 $3)))
      
      ;; literals
      (literals
       ((float-lit)          $1)
       ((double-lit)         $1)
       ((integer-lit)        $1)
       ((boolean-lit)        $1)
       ((string-lit)         $1)
       ((char-lit)           $1)
       ((null-lit)           null))
      
      ;; name
      ;; TODO: create ast structure for identifiers
      (name
       ((identifier)         $1)
       ((qualified-name)     $1))
      
      (qualified-name
       ((name period identifier) $1))
      
      ))))