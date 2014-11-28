(module parser racket

  (provide parse-processing)

  (require parser-tools/yacc
           parser-tools/lex
           syntax/readerr
           "mode.rkt"
           "lexer.rkt"
           "ast/ast.rkt"
           "ast/ast-type.rkt"
           "ast/ast-expr.rkt"
           "ast/ast-stmt.rkt"
           "ast/ast-class.rkt")

  (define (parse-processing src input-port)
    ;; turns on line and column location for input-port
    (port-count-lines! input-port)
    ;; lexer uses this for source location in case of error
    (file-path src) 
    ((processing-parser src) (lambda () (lex input-port))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Build Src
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-syntax (build-src stx)
    (syntax-case stx ()
      [(_ src-arg)
       (with-syntax 
         ([start-pos 
            (datum->syntax stx 
                           (string->symbol (format "$~a-start-pos" 
                                                   (syntax-e #'src-arg))))] 
          [end-pos 
            (datum->syntax stx 
                           (string->symbol (format "$~a-end-pos" 
                                                   (syntax-e #'src-arg))))]
          [src  (datum->syntax stx (string->symbol "src"))])
         #'(srcinfo->list src start-pos end-pos))]
      [(_ start-arg end-arg)
       (with-syntax 
         ([start-pos 
            (datum->syntax stx 
                           (string->symbol (format "$~a-start-pos" 
                                                   (syntax-e #'start-arg))))] 
          [end-pos 
            (datum->syntax stx 
                           (string->symbol (format "$~a-end-pos" 
                                                   (syntax-e #'end-arg))))]
          [src  (datum->syntax stx (string->symbol "src"))])
         #'(srcinfo->list src start-pos end-pos))]))

  ;;; Creates a list with all the source info
  (define (srcinfo->list src start end)
    (list src 
          (and start (position-line   start))
          (and start (position-col    start))
          (and start (position-offset start))
          (and start (- (position-offset end)
                        (position-offset start)))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Parser definition
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (processing-parser src)
    (parser
      (src-pos)
      (start <compilation-unit>)
      (end EOF)
      ;(debug "parser.debug")
      ;(yacc-output "yacc-output.debug")
      (error 
        (lambda (tok-ok? tok-name tok-value start-pos end-pos)
          (raise-read-error 
            (if tok-ok?
              (format "parse error with token: ~a, value: ~a" tok-name tok-value)
              (format "unrecognized token: ~a, value: ~a" tok-name tok-value))
            src
            (position-line   start-pos)
            (position-col    start-pos)
            (position-offset start-pos)
            (- (position-offset end-pos)
               (position-offset start-pos)))))
      (tokens operators literals seperators keywords empty-literals)

      (grammar

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Literals
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<literal>
          [(int-lit)      
           (make-object literal% $1 'int (build-src 1))] 
          [(long-lit)     
           (make-object literal% $1 'long (build-src 1))] 
          [(float-lit)    
           (make-object literal% $1 'float (build-src 1))] 
          [(double-lit)   
           (make-object literal% $1 'double (build-src 1))] 
          [(boolean-lit)  
           (make-object literal% $1 'boolean (build-src 1))] 
          [(string-lit)   
           (make-object literal% $1 'string (build-src 1))]
          [(char-lit)     
           (make-object literal% $1 'char (build-src 1))] 
          [(null-lit)     
           (make-object literal% null 'null (build-src 1))] 
          [(color-lit)    
           (make-object literal% $1 'color (build-src 1))])

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Types
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<type>
          [(<primitive-type>) 
           (make-object primitive-type% $1 (build-src 1))]
          [(<reference-type>) 
           (make-object todo-node% $1 'reference-type%  (build-src 1))])

        (<primitive-type>
          [(<numeric-type>) $1]
          [(boolean) 'bool])

        (<numeric-type>
          [(<integral-type>) $1]
          [(<floating-type>) $1])

        (<integral-type>
          [(byte)   'byte]
          [(short)  'short]   
          [(int)    'int]
          [(long)   'long]
          [(char)   'char])

        (<floating-type>
          [(float)  'float]
          [(double) 'double])   

        (<reference-type>
          [(<name>)      
           (make-object todo-node% $1 'name (build-src 1))]
          [(<array-type>) 
           (make-object todo-node% $1 'array-type (build-src 1))])

        (<array-type>
          [(<primitive-type> <dims>) 
           (make-object todo-node% (list $1 $2) 'array-type (build-src 1))]
          [(<name> <dims>)           
           (make-object todo-node% (list $1 $2) 'array-type (build-src 1))])

        (<class-or-interface-type>
          [(<name>) $1])

        (<class-type>
          [(<class-or-interface-type>)
           (make-object todo-node% $1 'class-type (build-src 1))])

        (<interface-type>
          [(<class-or-interface-type>) 
           (make-object todo-node% $1 'iterface-type (build-src 1))])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Dims & Args
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<args>
          [(<expr>) (list $1)]
          [(<args> comma <expr>)  (cons $3 $1)])

        (<dim-exprs>
          [(<dim-expr>) 
           (list $1)]
          [(<dim-exprs> <dim-expr>) 
           (cons $2 $1)])

        (<dim-expr>
          [(l-sbrack <expr> r-sbrack) 
           (make-object todo-node% $2 'enclosing-sbrack (build-src 2))])

        (<dims>
          [(l-sbrack r-sbrack) 
           (make-object todo-node% null 'empty-sbrack (build-src 1))]
          [(<dims> l-sbrack r-sbrack) 
           (make-object todo-node% $1 'empty-sbrack (build-src 1))])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Name
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<name>
          [(identifier)         
           (make-object identifier% null $1 (build-src 1))]
          [(<qualified-name>) $1])

        (<qualified-name>
          [(<name> period identifier) 
           (make-object identifier%  
                        (append (list (send $1 get-id)) (send $1 get-list))
                        $3 
                        (build-src 1 3))])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Compilation unit
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<compilation-unit>
          [() null]
          [(<import-declarations> <global-declarations>) 
           (append (reverse $1) (reverse $2))]
          [(<global-declarations>) 
           (reverse $1)])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Global 
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<global-declarations>
          [(<global-declaration>) (list $1)]
          [(<global-declarations> <global-declaration>) (cons $2 $1)])

        (<global-declaration>
          [(<class-declaration>) $1]
          [(<global-member-declaration>) 
           (make-object global-decl% $1 (build-src 1))]
          [(<global-stmt>)  
           (make-object global-stmt% $1 (build-src 1))])

        (<global-stmt>
          [(<stmt>) $1])

        (<global-member-declaration>
          [(<global-var-declaration>)  $1]
          [(<global-function-declaration>) $1])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Imports
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<import-declarations>
          [(<import-declaration>) (list $1)]
          [(<import-declarations> <import-declaration>) (cons $2 $1)])

        (<import-declaration>
          [(<single-type-import-declaration>)     $1]
          [(<type-import-on-demand-declaration>)  $1]
          [(<require-import-declaration>)         $1])

        (<single-type-import-declaration>
          [(import <name> semicolon)
           (make-object todo-node% null 'import (build-src 2))])

        (<type-import-on-demand-declaration>
          [(import <name> period * semicolon)
           (make-object todo-node% null 'import (build-src 2))])

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Requires - extention to support racket modules
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<require-import-declaration>
          [(require semicolon) 
           (make-object require% $1 (build-src 1))])

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Class
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<modifiers>
          [(<modifier>)              (list $1)]
          [(<modifiers> <modifier>)  (cons $2 $1)])

        (<modifier>
          [(public)         'public]
          [(protected)      'protected]
          [(private)        'private]
          [(abstract)       'abstract]
          [(final)          'final]
          [(native)         'native]
          [(static)         'static]
          [(synchronized)   'synchronized]
          [(transient)      'transient]
          [(volatile)       'volatile])

        (<class-declaration>
          [(<modifiers> class identifier <super> <interfaces> <class-body>)
           (make-object todo-node% (list $1 $3 $4 $5) 'class-decl (build-src 1))]
          [(<modifiers> class identifier <super> <class-body>)
           (make-object todo-node% (list $1 $4 $5) 'class-decl (build-src 1))]
          [(<modifiers> class identifier <interfaces> <class-body>)
           (make-object todo-node% (list $1 $4 $5) 'class-decl (build-src 1))]
          [(<modifiers> class identifier <class-body>)
           (make-object todo-node% (list $1 $4) 'class-decl (build-src 1))]
          [(class identifier <super> <interfaces> <class-body>)
           (make-object todo-node% (list $3 $4 $5) 'class-decl (build-src 1))]
          [(class identifier <super> <class-body>)
           (make-object todo-node% (list $3 $4) 'class-decl (build-src 1))]
          [(class identifier <interfaces> <class-body>)
           (make-object todo-node% (list $3 $4) 'class-decl (build-src 1))]
          [(class identifier <class-body>)
           (make-object class-node% (make-object identifier% null $2 (build-src 2)) $3 (build-src 1))])

        (<super>
          [(extends <class-type>)
           (make-object todo-node% $2 'super-class-type (build-src 1))])

        (<interfaces>
          [(implements <interface-type-list>)
           (make-object todo-node% $2 'implments-types (build-src 1))])

        (<interface-type-list>
          [(<interface-type>) (list $1)]
          [(<interface-type-list> comma <interface-type>) (cons $3 $1)])

        (<class-body>
          [(l-cbrack <class-body-declarations> r-cbrack) (reverse $2)])

        (<class-body-declarations>
          [(<class-body-declaration>) (list $1)]
          [(<class-body-declarations> <class-body-declaration>) (cons $2 $1)])


        (<class-body-declaration>
          [(<class-member-declaration>) $1]
          [(<constructor-declaration>) $1])

        (<class-member-declaration>
          [(<field-declaration>) $1]
          [(<method-declaration>) $1])

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Methods
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<global-function-declaration>
          [(<method-header> <method-body>) 
           (begin 
             ; Changes from static mode to active mode: see mode.rkt
             (active-mode)
             (make-object function-decl% $1 $2 (build-src 1 2)))])

        (<method-declaration>
          [(<method-header> <method-body>) 
           (begin 
             ; Changes from static mode to active mode: see mode.rkt
             (active-mode)
             (make-object method-decl% $1 $2 (build-src 1 2)))])

        (<method-header>
          [(<modifiers> <type> <method-declarator> <throws>) 
           (make-object method-header% $1 $2 (car $3) (cdr $3) $4 (build-src 3))]
          [(<modifiers> <type> <method-declarator>) 
           (make-object method-header% $1 $2 (car $3) (cdr $3) null (build-src 3))]
          [(<type> <method-declarator> <throws>) 
           (make-object method-header% null $1 (car $2) (cdr $2) $3 (build-src 2))]
          [(<type> <method-declarator>) 
           (make-object method-header% null $1 (car $2) (cdr $2) null (build-src 2))]
          [(<modifiers> <void> <method-declarator> <throws>) 
           (make-object method-header% $1 $2 (car $3) (cdr $3) $4 (build-src 3))]
          [(<modifiers> <void> <method-declarator>) 
           (make-object method-header% $1 $2 (car $3) (cdr $3) null (build-src 3))]
          [(<void> <method-declarator> <throws>) 
           (make-object method-header% null $1 (car $2) (cdr $2) $3 (build-src 2))]
          [(<void> <method-declarator>) 
           (make-object method-header% null $1 (car $2) (cdr $2) null (build-src 2))])

        (<void>
          [(void)   (make-object primitive-type% 'void (build-src 1))])

        (<method-declarator>
          [(identifier l-paren <formal-parameter-list> r-paren) 
           (cons (make-object identifier% null $1 (build-src 1)) $3)]
          [(identifier l-paren r-paren) 
           (cons (make-object identifier% null $1 (build-src 1)) null)]
          [(identifier l-paren <formal-parameter-list> r-paren <dims>) 
           (make-object todo-node% (list $3 $5) 'method-declarator (build-src 2))]
          [(identifier l-paren r-paren <dims>) 
           (make-object todo-node% $4 'method-declarator (build-src 2))])

        (<formal-parameter-list>
          [(<formal-parameter>) (list $1)]
          [(<formal-parameter-list> comma <formal-parameter>) (cons $3 $1)])

        (<formal-parameter>
          [(<type> <var-decl-id>)
           (make-object formal-parameter% null $1 $2 (build-src 1))]
          [(final <type> <var-decl-id>) 
           (make-object formal-parameter% 'final $2 $3 (build-src 2))])

        (<throws>
          [(throws <class-type-list>) 
           (make-object todo-node% $2 'throws (build-src 2))])

        (<class-type-list>
          [(<class-type>) (list $1)]
          [(<class-type-list> comma <class-type>) (cons $3 $1)])

        (<method-body>
          [(<block>)   $1]
          [(semicolon) null])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Constructors 
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<constructor-declaration>
          [(<modifiers> <constructor-declarator> <throws> <constructor-body>)
           (make-object todo-node% (list $1 $2 $3 $4) 'construct-decl (build-src 1))]
          [(<modifiers> <constructor-declarator> <constructor-body>)
           (make-object todo-node% (list $1 $2 $3) 'construct-decl (build-src 1))]
          [(<constructor-declarator> <throws> <constructor-body>)
           (make-object todo-node% (list $1 $2 $3) 'construct-decl (build-src 1))]
          [(<constructor-declarator> <constructor-body>)
           (make-object todo-node% (list $1 $2) 'construct-decl (build-src 1))])

        (<constructor-declarator>
          [(identifier l-paren <formal-parameter-list> r-paren) 
           (make-object todo-node% $3 'construct-declarator (build-src 1))]
          [(identifier l-paren r-paren) 
           (make-object todo-node% null 'construct-declarator (build-src 1))])

        (<constructor-body>
          [(l-cbrack <explicit-constructor-invocation> <block-stmts> r-cbrack)
           (make-object todo-node% (list $2 $3) 'construct-body (build-src 2))]
          [(l-cbrack <explicit-constructor-invocation> r-cbrack)
           (make-object todo-node% $2 'construct-body (build-src 2))]
          [(l-cbrack <block-stmts> r-cbrack)
           (make-object todo-node% $2 'construct-body (build-src 2))]
          [(l-cbrack r-cbrack)
           (make-object todo-node% null 'construct-body (build-src 2))])

        (<explicit-constructor-invocation>
          [(this l-paren <args> r-paren semicolon)
           (make-object todo-node% $3 'constrct-invocation (build-src 1))]
          [(this l-paren r-paren semicolon)
           (make-object todo-node% null 'constrct-invocation (build-src 1))]
          [(super l-paren <args> r-paren semicolon)
           (make-object todo-node% $3 'constrct-super-invocation (build-src 1))]
          [(super l-paren r-paren semicolon)
           (make-object todo-node% null 'constrct-super-invocation (build-src 1))])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Array init
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<array-initializer>
          [(l-sbrack <var-initializers> comma r-sbrack)   
           (make-object todo-node% $2 'array-init (build-src 2))]
          [(l-sbrack <var-initializers> r-sbrack)         
           (make-object todo-node% $2 'array-init (build-src 2))]
          [(l-sbrack comma r-sbrack)                    
           (make-object todo-node% null 'array-init (build-src 1))]
          [(l-sbrack r-sbrack)     
           (make-object todo-node% null 'array-init (build-src 1))])

        (<var-initializers>
          [(<var-initializer>)       
           (list $1)]
          [(<var-initializers> comma <var-initializer>) 
           (cons $3 $1)])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Fields
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<global-var-declaration>
          [(<modifiers> <type> <var-declarators> semicolon)
           (make-object global-var% $1 $2 (reverse $3)  (build-src 1 4))]
          [(<type> <var-declarators> semicolon)
           (make-object global-var% null $1 (reverse $2)  (build-src 1 3))])

        (<field-declaration>
          [(<modifiers> <type> <var-declarators> semicolon)
           (make-object class-field% $1 $2 (reverse $3)  (build-src 1 4))]
          [(<type> <var-declarators> semicolon)
           (make-object class-field% null $1 (reverse $2)  (build-src 1 3))])

        (<var-declarators>
          [(<var-declarator>) (list $1)]
          [(<var-declarators> comma <var-declarator>) (cons $3 $1)])

        (<var-declarator>
          [(<var-decl-id>) 
           (make-object var-decl-id% $1 (make-object undefined% null) (build-src 1))]
          [(<var-decl-id> = <var-initializer>) 
           (make-object var-decl-id% $1 $3 (build-src 1 3))])

        (<var-decl-id> 
          [(identifier)
           (make-object identifier% null $1 (build-src 1))]
          [(identifier <dims>)
           (make-object todo-node% $1 'var-declartor-id (build-src 1))])

        (<var-initializer>
          [(<expr>) $1]
          [(<array-initializer>) $1])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Statements
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<block>
          [(l-cbrack <block-stmts> r-cbrack)  
           (make-object block% (reverse $2) (build-src 1))]
          [(l-cbrack r-cbrack)              
           (make-object block% null (build-src 1))])

        (<block-stmts>
          [(<block-stmt>)  (list $1)]
          [(<block-stmts> <block-stmt>) (cons $2 $1)])

        (<block-stmt>
          [(<local-var-decl-stmt>) $1]
          [(<stmt>) $1])

        (<local-var-decl-stmt>
          [(<local-var-decl> semicolon) $1])

        (<local-var-decl>
          [(<modifiers> <type> <var-declarators>) 
           (make-object local-var% $1 $2 (reverse $3)  (build-src 1 3))]
          [(<type> <var-declarators>) 
           (make-object local-var% null $1 (reverse $2)  (build-src 1 2))])

        (<stmt>
          [(<stmt-no-trailing-substmt>) $1]
          [(<labeled-stmt>) $1]
          [(<if-then-stmt>) $1]
          [(<if-then-else-stmt>) $1]
          [(<while-stmt>) $1]
          [(<for-stmt>) $1])

        (<stmt-no-short-if>
          [(<stmt-no-trailing-substmt>) $1]
          [(<labeled-stmt-no-short-if>) $1]
          [(<if-then-else-stmt-no-short-if>) $1]
          [(<while-stmt-no-short-if>) $1]
          [(<for-stmt-no-short-if>) $1])

        (<stmt-no-trailing-substmt>
          [(<block>) $1]
          [(<empty-stmt>) $1]
          [(<expr-stmt>) $1]
          [(<switch-stmt>) $1]
          [(<do-stmt>) $1]
          [(<break-stmt>) $1]
          [(<continue-stmt>) $1]
          [(<return-stmt>) $1]
          [(<synchronized-stmt>) $1]
          [(<throw-stmt>) $1]
          [(<try-stmt>) $1])

        (<empty-stmt>
          [(semicolon) 
           (make-object empty-stmt% (build-src 1))])

        (<expr-stmt>
          [(<stmt-expr>  semicolon) $1])

        (<stmt-expr>
          [(<assignment>) $1]
          [(<pre-inc-expr>) $1]
          [(<pre-dec-expr>) $1]
          [(<post-inc-expr>) $1]
          [(<post-dec-expr>) $1]
          [(<method-call>) $1]
          ;; TODO: check if color in processing is a stmt-expr
          [(<color-instance-creation>) $1]
          [(<class-instance-creation-expr>) $1])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Loop stmts
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<do-stmt>
          [(do <stmt> while l-paren <expr> r-paren semicolon)
           (make-object do-while% $5 $2 (build-src 1))])

        (<while-stmt>
          [(while l-paren <expr> r-paren <stmt>)
           (make-object while% $3 $5 (build-src 1))])

        (<while-stmt-no-short-if>
          [(while l-paren <expr> r-paren <stmt-no-short-if>)
           (make-object while% $3 $5 (build-src 1))])

        (<for-stmt>
          [(for l-paren <finit> semicolon <expr> semicolon <fupdate> r-paren <stmt>)
           (make-object for% $3 $5 $7 $9 (build-src 1))]
          [(for l-paren <finit> semicolon semicolon <fupdate> r-paren <stmt>)
           (make-object for% $3 null $8 (build-src 1))])

        (<for-stmt-no-short-if>
          [(for l-paren <finit> semicolon <expr> semicolon <fupdate> r-paren <stmt-no-short-if>)
           (make-object for% $3 $5 $7 $9 (build-src 1))]
          [(for l-paren <finit> semicolon semicolon <fupdate> r-paren <stmt-no-short-if>)
           (make-object for% $3 null $8 (build-src 1))])

        (<finit>
          [() (make-object empty-stmt% null)]
          [(<local-var-decl>) $1]
          [(<stmt-expr-list>) 
           (make-object expr-list% (reverse $1) (build-src 1))])

        (<fupdate>
          [() (make-object empty-stmt% null)]
          [(<stmt-expr-list>) 
           (make-object expr-list% (reverse $1) (build-src 1))])

        (<stmt-expr-list>
          [(<stmt-expr>) (list $1)]
          [(<stmt-expr-list> comma <stmt-expr>) (cons $3 $1)])
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Control stmts
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<if-then-stmt>
          [(if l-paren <expr> r-paren <stmt>) 
           (make-object if% $3 $5 null (build-src 1))])

        (<if-then-else-stmt>
          [(if l-paren <expr> r-paren <stmt-no-short-if> else <stmt>)
           (make-object if% $3 $5 $7 (build-src 1))])

        (<if-then-else-stmt-no-short-if>
          [(if l-paren <expr> r-paren <stmt-no-short-if> else <stmt-no-short-if>)
           (make-object if% $3 $5 $7 (build-src 1))])

        (<labeled-stmt>
          [(identifier : <stmt>) 
           (make-object todo-node% $3 'labeld-stmt (build-src 1))])

        (<labeled-stmt-no-short-if>
          [(identifier : <stmt-no-short-if>) 
           (make-object todo-node% $3 'labeld-stmt-no-if (build-src 1))])

        (<switch-stmt>
          [(switch l-paren <expr> r-paren <switch-block>)
           (make-object todo-node% (list $3 $5) 'switch-stmt (build-src 1))])

        (<switch-block>
          [(l-cbrack <switch-block-stmt-groups> <switch-labels> r-cbrack)
           (make-object todo-node% (list $2 $3) 'switch-block (build-src 2))]
          [(l-cbrack <switch-block-stmt-groups> r-cbrack) 
           (make-object todo-node% $2 'switch-block (build-src 2))]
          [(l-cbrack <switch-labels> r-cbrack)
           (make-object todo-node% $2 'switch-block (build-src 2))]
          [(l-cbrack r-cbrack) 
           (make-object todo-node% null 'switch-block (build-src 2))])

        (<switch-block-stmt-groups>
          [(<switch-block-stmt-group>) (list $1)]
          [(<switch-block-stmt-groups> <switch-block-stmt-group>) (cons $2 $1)])

        (<switch-block-stmt-group>
          [(<switch-labels> <block-stmts>) 
           (make-object todo-node% (list $1 $2) 'switch-block-stmt-group (build-src 1))])

        (<switch-labels>
          [(<switch-label>) $1]
          [(<switch-labels> <switch-label>) (cons $2 $1)])

        (<switch-label>
          [(case <constant-expr> :) 
           (make-object todo-node% $2 'switch-case (build-src 1))]
          [(default :)
           (make-object todo-node% null 'switch-default (build-src 1))])

        (<break-stmt>
          [(break identifier semicolon) 
           (make-object todo-node% null 'break (build-src 1))]
          [(break semicolon) 
           (make-object break% (build-src 1))])

        (<continue-stmt>
          [(continue identifier semicolon) 
           (make-object todo-node% null 'continue (build-src 1))]
          [(continue semicolon) 
           (make-object continue% (build-src 1))])

        (<return-stmt>
          [(return <expr> semicolon) 
           (make-object return% $2 (build-src 1))]
          [(return semicolon) 
           (make-object return% null (build-src 1))])

        (<synchronized-stmt>
          [(synchronized l-paren <expr> r-paren <block>)
           (make-object todo-node% (list $3 $5) 'synchronized (build-src 1))])

        (<throw-stmt>
          [(throw <expr> semicolon) 
           (make-object todo-node% $2 'throw (build-src 1))])

        (<try-stmt>
          [(try <block> <catches>) 
           (make-object todo-node% (list $2 $3) 'try (build-src 1))]
          [(try <block> <catches> <finally-stmt>) 
           (make-object todo-node% (list $2 $3 $4) 'try (build-src 1))]
          [(try <block> <finally-stmt>) 
           (make-object todo-node% (list $2 $3) 'try (build-src 1))])

        (<catches>
          [(<catch-clause>) (list $1)]
          [(<catches> <catch-clause>) (cons $2 $1)])

        (<catch-clause>
          [(catch l-paren <formal-parameter> r-paren <block>)
           (make-object todo-node% (list $3 $5) 'catch (build-src 1))])

        (<finally-stmt>
          [(finally <block>) 
           (make-object todo-node% $2 'finally-stmt (build-src 1))])

        (<primary>
          [(<primary-no-new-array>) $1]
          [(<array-creation-expr>) $1])

        (<primary-no-new-array>
          [(<literal>) $1]
          [(this) 
           (make-object this-node% (build-src 1))]
          [(l-paren <expr> r-paren) $2]
          [(<class-instance-creation-expr>) $1]
          [(<field-access>) $1]
          [(<method-call>) $1]
          [(<array-access>) 
           (error "Not Implemented")]
          [(<name> period this) 
           (error "Not Implemented")])

        (<field-access>
          [(<primary> period identifier)    
           (make-object todo-node% $1 'primary-field-access (build-src 1))]
          [(super period identifier)     
           (make-object todo-node% null 'super-field-access (build-src 1))]
          [(<name> period super period identifier)     
           (make-object todo-node% $1 'name-super-field-access (build-src 1))])

        (<array-access>
          [(<name> l-sbrack <expr> r-sbrack)
           (make-object todo-node% (list $1 $3) 'array-access (build-src 1))]
          [(<primary-no-new-array> l-sbrack <expr> r-sbrack)
           (make-object todo-node% (list $1 $3) 'array-access (build-src 1))])


        (<method-call>
          [(<name> l-paren <args> r-paren) 
           (make-object method-call% 
                        $1 
                        (make-object arguments% $3 (build-src 3))
                        (build-src 1 4))]
          [(<name> l-paren r-paren) 
           (make-object method-call% $1 null (build-src 1 3))]

          ;; TODO: Solve the name resolution
          [(<primary> period identifier l-paren <args> r-paren)
           (make-object todo-node% (list $1 $5) 'method-call (build-src 1))]
          [(<primary> period identifier l-paren r-paren)
           (make-object todo-node% $1 (build-src 1))]
          [(super period identifier l-paren <args> r-paren)
           (make-object todo-node% $5 'super-method-call (build-src 1))]
          [(super period identifier l-paren r-paren)
           (make-object todo-node% null 'super-method-call (build-src 1))]
          [(<name> period super period identifier l-paren <args> r-paren)
           (make-object todo-node% (list $1 $7) 'method-call (build-src 1))]
          [(<name> period super period identifier l-paren r-paren)
           (make-object todo-node% $1 'method-call (build-src 1))])

        (<class-instance-creation-expr>
          [(new <class-or-interface-type> l-paren r-paren)
           (make-object new-node% $2 (build-src 1))]

          ;; TODO
          [(new <class-or-interface-type> l-paren <args> r-paren <class-body>)
           (make-object todo-node% (list $2 $4 $6) 'new (build-src 1))]
          [(new <class-or-interface-type> l-paren r-paren <class-body>)
           (make-object todo-node% (list $2 $5) 'new (build-src 1))]
          [(new <class-or-interface-type> l-paren <args> r-paren)
           (make-object todo-node% (list $2 $4) 'new (build-src 1))]


          [(<primary> period new identifier l-paren <args> r-paren <class-body>)
           (make-object todo-node% (list $1 $6 $8) 'new (build-src 1))]
          [(<primary> period new identifier l-paren r-paren <class-body>)
           (make-object todo-node% (list $1 $7) 'new (build-src 1))]
          [(<primary> period new identifier l-paren <args> r-paren)
           (make-object todo-node% (list $1 $6) 'new (build-src 1))]
          [(<primary> period new identifier l-paren r-paren)
           (make-object todo-node% $1 'new (build-src 1))]

          [(<name> period new identifier l-paren <args> r-paren <class-body>)
           (make-object todo-node% (list $1 $6 $8) 'new (build-src 1))]
          [(<name> period new identifier l-paren r-paren <class-body>)
           (make-object todo-node% (list $1 $7) 'new (build-src 1))]
          [(<name> period new identifier l-paren <args> r-paren)
           (make-object todo-node% (list $1 $6) 'new (build-src 1))]
          [(<name> period new identifier l-paren r-paren)
           (make-object todo-node% $1 'new (build-src 1))])

        (<color-instance-creation>
          [(color l-paren <args> r-paren) 
           (make-object todo-node% $3 'color (build-src 1))])

        (<array-creation-expr>
          [(new <primitive-type> <dim-exprs> <dims>) 
           (make-object todo-node% (list $2 $3 $4) 'new-array (build-src 1))]
          [(new <primitive-type> <dim-exprs>) 
           (make-object todo-node% (list $2 $3) 'new-array (build-src 1))]
          [(new <primitive-type> <dims> <array-initializer>) 
           (make-object todo-node% (list $2 $3 $4) 'new-array (build-src 1))]
          [(new <class-or-interface-type> <dim-exprs> <dims>) 
           (make-object todo-node% (list $2 $3 $4) 'new-array (build-src 1))]
          [(new <class-or-interface-type> <dim-exprs>) 
           (make-object todo-node% (list $2 $3) 'new-array (build-src 1))]
          [(new <class-or-interface-type> <dims> <array-initializer>) 
           (make-object todo-node% (list $2 $3 $4) 'new-array (build-src 1))])

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Expressions and Assignments
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (<assignment>
          [(<left-hand> <assignment-operator> <assignment-expr>)
           (make-object assignment% $2 $1 $3 (build-src 1))])

        (<left-hand>
          [(<name>)  $1]
          [(<field-access>)
           (make-object todo-node% $1 'left-hand (build-src 1))]
          [(<array-access>) 
           (make-object todo-node% $1 'left-hand (build-src 1))])

        (<postfix-expr>
          [(<primary>) $1]
          [(<name>) $1]
          [(<post-inc-expr>) $1]
          [(<post-dec-expr>) $1])

        (<post-inc-expr>
          [(<postfix-expr> ++) 
           (make-object todo-node% $1 'post++ (build-src 1))])

        (<post-dec-expr>
          [(<postfix-expr> --) 
           (make-object todo-node% $1 'post-- (build-src 1))])

        (<unary-expr>
          [(<pre-inc-expr>) $1]
          [(<pre-dec-expr>) $1]
          [(+ <unary-expr>) 
           (make-object unary-op% '+ $2  (build-src 2))]
          [(- <unary-expr>) 
           (make-object unary-op% '- $2  (build-src 2))]
          [(<unary-expr-not-plus-minus>) $1])

        (<pre-inc-expr>
          [(++ <unary-expr>) 
           (make-object todo-node% $2 'pre++  (build-src 1))])

        (<pre-dec-expr>
          [(-- <unary-expr>) 
           (make-object todo-node% $2  'pre-- (build-src 1))])

        (<unary-expr-not-plus-minus>
          [(<postfix-expr>) $1]
          [(~ <unary-expr>) 
           (make-object unary-op% 'not $2  (build-src 2))]
          [(! <unary-expr>) 
           (make-object unary-op% '! $2  (build-src 2))]
          [(<cast-expr>) $1])

        (<cast-expr>
          [(l-paren <primitive-type> <dims> r-paren <unary-expr>)
           (make-object todo-node% (list $2 $3 $5) 'cast-expr (build-src 2))]
          [(l-paren <primitive-type> r-paren <unary-expr>)
           (make-object todo-node% (list $2 $4) 'cast-expr (build-src 2))]
          [(l-paren <expr> r-paren <unary-expr-not-plus-minus>)
           (make-object todo-node% (list $2 $4) 'cast-expr (build-src 2))]
          [(l-paren <name> <dims> r-paren <unary-expr-not-plus-minus>)
           (make-object todo-node% (list $2 $3 $5) 'cast-expr (build-src 2))])

        (<multiplicative-expr>
          [(<unary-expr>) $1]
          [(<multiplicative-expr> * <unary-expr>)
           (make-object binary-op% '* $1 $3 (build-src 1))]
          [(<multiplicative-expr> / <unary-expr>)
           (make-object binary-op% '/ $1 $3 (build-src 1))]
          [(<multiplicative-expr> % <unary-expr>)
           (make-object binary-op% '% $1 $3 (build-src 1))])

        (<additive-expr>
          [(<multiplicative-expr>) $1]
          [(<additive-expr> + <multiplicative-expr>)
           (make-object binary-op% '+ $1 $3 (build-src 1))]
          [(<additive-expr> - <multiplicative-expr>)
           (make-object binary-op% '- $1 $3 (build-src 1))])

        (<shift-expr>
          [(<additive-expr>) $1]
          [(<shift-expr> << <additive-expr>)
           (make-object binary-op% '<< $1 $3 (build-src 1))]
          [(<shift-expr> >> <additive-expr>)
           (make-object binary-op% '>> $1 $3 (build-src 1))]
          [(<shift-expr> >>> <additive-expr>)
           (make-object binary-op% '>>> $1 $3 (build-src 1))])

        (<relational-expr>
          [(<shift-expr>) $1]
          [(<shift-expr> < <shift-expr>)
           (make-object binary-op% '< $1 $3 (build-src 1))]
          [(<relational-expr> > <shift-expr>)
           (make-object binary-op% '> $1 $3 (build-src 1))]
          [(<relational-expr> <= <shift-expr>)
           (make-object binary-op% '<= $1 $3 (build-src 1))]
          [(<relational-expr> >= <shift-expr>)
           (make-object binary-op% '>= $1 $3 (build-src 1))]
          [(<relational-expr> instanceof <reference-type>)
           (make-object binary-op% 'instanceof $1 $3 (build-src 1))])

        (<equality-expr>
          [(<relational-expr>) $1]
          [(<equality-expr> == <relational-expr>)
           (make-object binary-op% '== $1 $3 (build-src 1))]
          [(<equality-expr> != <relational-expr>)
           (make-object binary-op% '!= $1 $3 (build-src 1))])

        (<and-expr>
          [(<equality-expr>) $1]
          [(<and-expr> & <equality-expr>)
           (make-object binary-op% '& $1 $3 (build-src 1))])

        (<exclusive-or-expr>
          [(<and-expr>) $1]
          [(<exclusive-or-expr> ^ <and-expr>)
           (make-object binary-op% '^ $1 $3 (build-src 1))])

        (<inclusive-or-expr>
          [(<exclusive-or-expr>) $1]
          [(<inclusive-or-expr> PIPE <exclusive-or-expr>)
           (make-object binary-op% 'pipe $1 $3 (build-src 1))])

        (<conditional-and-expr>
          [(<inclusive-or-expr>) $1]
          [(<conditional-and-expr> && <inclusive-or-expr>)
           (make-object binary-op% '&& $1 $3 (build-src 1))])

        (<conditional-or-expr>
          [(<conditional-and-expr>) $1]
          [(<conditional-or-expr> OR <conditional-and-expr>)
           (make-object binary-op% 'or $1 $3 (build-src 1))])

        (<conditional-expr>
          [(<conditional-or-expr>) $1]
          [(<conditional-or-expr> ? <expr> : <conditional-expr>)
           (make-object todo-node% (list $1 $3 $5) 'ternary-op (build-src 1))])

        (<assignment-expr>
          [(<conditional-expr>) $1]
          [(<assignment>) $1])

        (<assignment-operator>
          [(=)   '=]
          [(*=)  '*=]
          [(/=)  '/=]
          [(%=)  '%=]
          [(+=)  '+=]
          [(-=)  '-=]
          [(&=)  '&=]
          [(^=)  '^=]
          [(<<=) '<<=]
          [(>>=) '>>=]
          [(>>>=) '>>>=]
          [(OREQUAL) 'or=])

        (<expr>
          [(<assignment-expr>) $1])

        (<constant-expr>
          [(<expr>) $1])))))
