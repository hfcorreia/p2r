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
       [(stmt)           $1]
       [(stmt stmts)     (make-stmts $1 $2)]) 
      
      (stmt
       [(modifiers semicolon)         (make-stmt $1)]
       [(expr semicolon)              (make-stmt $1)])
      
      (expr  
       [(literals)       (make-num-exp $1)]
       [(expr + expr)    (make-arith-exp + $1 $3)]
       [(expr - expr)    (make-arith-exp - $1 $3)]
       [(expr * expr)    (make-arith-exp * $1 $3)]
       [(expr / expr)    (make-arith-exp / $1 $3)])
      
     ;; literals
     (literals
       [(int-lit)            (make-literal 'int $1)]
       [(long-lit)           (make-literal 'long $1)]
       [(float-lit)          (make-literal 'float $1)]
       [(double-lit)         (make-literal 'double $1)]
       [(boolean-lit)        (make-literal 'boolean $1)]
       [(string-lit)         (make-literal 'string $1)]
       [(char-lit)           (make-literal 'char $1)]
       [(null-lit)           (make-literal 'null null)])

     ;; modifiers
     (modifiers
       [(modifier)              (list $1)]
       [(modifiers modifier)    (cons $2 $1)])


     (modifier
       [(public)        (make-modifier 'public )]
       [(protected)     (make-modifier 'protected )]
       [(private)       (make-modifier 'private )]
       [(static)        (make-modifier 'static )]
       [(abstract)      (make-modifier 'abstract )]
       [(final)         (make-modifier 'final )]
       [(strictfp)      (make-modifier 'strictfp )]
       [(native)        (make-modifier 'native )]
       [(synchronized)  (make-modifier 'synchronized )]
       [(transient)     (make-modifier 'transient )]
       [(volatile)      (make-modifier 'volatile )])

     ;; types
      (type
       [(primitive-type) $1]
       [(reference-type) $1])
      
      (primitive-type
       [(numeric-type) $1]
       [(boolean)      (make-type 'boolean 0 )])
      
      (numeric-type
       [(integral-type) $1]
       [(floating-type) $1])
      
      (integral-type
       [(byte)      (make-type 'byte 0 )]
       [(short)     (make-type 'short 0 )]
       [(int)       (make-type 'int 0 )]
       [(long)      (make-type 'long 0 )]
       [(char)      (make-type 'char 0 )])
      
      (floating-type
       [(float)     (make-type 'float 0 )]
       [(double)    (make-type 'double 0 )])
      
      (reference-type
       [(name)      (make-type $1 0 )]
       [(array-type) $1])
            
      (class-type
       [(name) $1])
      
      (interface-type
       [(name) $1])
      
      (array-type
       [(primitive-type dims)   (make-type (type-name $1) $2 )]
       [(name dims)             (make-type $1 $2)])

      (dims
        [(l-sbrack r-sbrack) 1]
        [(dims l-sbrack r-sbrack) (add1 $1)])

     ;; name
     (name
       [(identifier)         $1]
       [(qualified-name)     $1])

     (qualified-name
       [(name period identifier) $1])

     ))))
