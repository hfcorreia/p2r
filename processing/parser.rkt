(module processing-parser racket
  (require parser-tools/yacc
           parser-tools/lex
           syntax/readerr
           "lexer.rkt"
           "ast.rkt")

  (provide parse-processing)

  (define (parse-processing src port)
    (port-count-lines! port)
    ((processing-parser src) (lambda () (lex port))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Build Src
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-syntax (build-src stx)
    (syntax-case stx ()
      [(_ src-arg)
       (with-syntax 
         ([start-pos (datum->syntax stx (string->symbol (format "$~a-start-pos" (syntax-e #'src-arg))))] 
          [end-pos (datum->syntax stx (string->symbol (format "$~a-end-pos" (syntax-e #'src-arg))))]
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Parser definition
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (processing-parser src)
    (parser
      (src-pos)
      (start compilation-unit)
      (end EOF)
      (debug "parser.debug")
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
        ;; Literals
        (literal
          [(int-lit)            (make-object todo-node% $1 (srcinfo->list src $1-start-pos $1-end-pos))]
          [(long-lit)           (make-object todo-node% $1 (build-src 1))]
          [(float-lit)          (make-object todo-node% $1 (build-src 1))] 
          [(double-lit)         (make-object todo-node% $1 (build-src 1))] 
          [(boolean-lit)        (make-object todo-node% $1 (build-src 1))] 
          [(string-lit)         (make-object todo-node% $1 (build-src 1))] 
          [(char-lit)           (make-object todo-node% $1 (build-src 1))] 
          [(null-lit)           (make-object todo-node% null (build-src 1))]
          [(color-lit)          (make-object todo-node% $1 (build-src 1))])

        ;; Types
        (type
          [(primitive-type) $1]
          [(reference-type) $1])

        (primitive-type
          [(numeric-type) $1]
          [(boolean)      (make-object todo-node% 'boolean (build-src 1))])

        (numeric-type
          [(integral-type) $1]
          [(floating-type) $1])

        (integral-type
          [(byte)      (make-object todo-node%  'byte  (build-src 1))]
          [(short)     (make-object todo-node%  'short (build-src 1))]
          [(int)       (make-object todo-node%  'int   (build-src 1))]
          [(long)      (make-object todo-node%  'long  (build-src 1))]
          [(char)      (make-object todo-node%  'char  (build-src 1))])

        (floating-type
          [(float)     (make-object todo-node% 'float  (build-src 1))]
          [(double)    (make-object todo-node% 'double (build-src 1))])

        (reference-type
          [(name)      (make-object todo-node% $1 (build-src 1))]
          [(array-type) $1])

        (class-or-interface-type
          [(name) $1])

        (class-type
          [(class-or-interface-type) $1])

        (interface-type
          [(class-or-interface-type) $1])

        (array-type
          [(primitive-type dims)   (make-object todo-node% $1 (build-src 1))]
          [(name dims)             (make-object todo-node% $1 (build-src 1))])

        (dims
          [(l-sbrack r-sbrack) 1]
          [(dims l-sbrack r-sbrack) (add1 $1)])

        ;; Name
        (name
          [(identifier)         $1]
          [(qualified-name)     $1])

        (qualified-name
          [(name period identifier) $1])

        ;; Compilation unit 
        (compilation-unit
          [(package-declaration import-declarations type-declarations)
           (make-object todo-node% $1 (build-src 1))]
          [(import-declarations type-declarations) (make-object todo-node% $1 (build-src 1))]
          [(package-declaration type-declarations) (make-object todo-node% $1 (build-src 1))]
          [(package-declaration import-declarations) (make-object todo-node% $1 (build-src 1))]
          [(package-declaration) (make-object todo-node% $1 (build-src 1))]
          [(import-declarations) (make-object todo-node% $1 (build-src 1))]
          [(type-declarations) (make-object todo-node% $1 (build-src 1))]
          [() (make-object todo-node% null null)])

        (import-declarations
          [(import-declaration) (list $1)]
          [(import-declarations import-declaration) (make-object todo-node% $1 (build-src 1))])

        (type-declarations
          [(type-declaration) (list $1)]
          [(type-declarations type-declaration) (make-object todo-node% $1 (build-src 1))])

        (package-declaration
          [(package name semicolon) $2])

        (import-declaration
          [(single-type-import-declaration)     $1]
          [(type-import-on-demand-declaration)  $1])

        (single-type-import-declaration
          [(import name semicolon)
           (make-object todo-node% $2 (build-src 2))])

        (type-import-on-demand-declaration
          [(import name period * semicolon)
           (make-object todo-node% $2 (build-src 2))])

        (type-declaration
          [(class-declaration)    $1]
          [(interface-declaration)    $1]
          [(semicolon) #f])

        ;; Modifiers
        (modifiers
          [(modifier)              (list $1)]
          [(modifiers modifier)    (cons $2 $1)])

        (modifier
          [(public)        (make-object todo-node% 'public (build-src 1))]
          [(protected)     (make-object todo-node% 'protected (build-src 1))]
          [(private)       (make-object todo-node% 'private (build-src 1))]
          [(static)        (make-object todo-node% 'static (build-src 1))]
          [(abstract)      (make-object todo-node% 'abstract (build-src 1))]
          [(final)         (make-object todo-node% 'final (build-src 1))]
          [(strictfp)      (make-object todo-node% 'strictfp (build-src 1))]
          [(native)        (make-object todo-node% 'native (build-src 1))]
          [(synchronized)  (make-object todo-node% 'synchronized (build-src 1))]
          [(transient)     (make-object todo-node% 'transient (build-src 1))]
          [(volatile)      (make-object todo-node% 'volatile (build-src 1))])


        ;; Class
        (class-declaration
          [(modifiers class identifier super-class interfaces class-body)
           (make-object todo-node% $1 (build-src 1))]
          [(class identifier super-class interfaces class-body)
           (make-object todo-node% $2 (build-src 2))])

        (super-class
          [() null]
          [(extends class-type) (list $2)])

        (interfaces
          [() null]
          [(implements interface-type-list) $2])

        (interface-type-list
          [(interface-type) (list $1)]
          [(interface-type-list comma interface-type) (cons $3 $1)])

        (class-body
          [(l-cbrack class-body-declarations r-cbrack) (reverse $2)])

        (class-body-declarations
          [() null]
          [(class-body-declarations class-body-declaration)
           (cond
             ((not $2) $1)
             ((list? $2) (append $2 $1))
             (else (cons $2 $1)))])

        (class-body-declaration
          [(class-member-declaration) $1]
          [(class-declaration) $1]
          [(interface-declaration) $1]
          [(static-initializer) $1]
          [(constructor-declaration) $1]
          [(semicolon) #f])

        (class-member-declaration
          [(field-declaration) $1]
          [(method-declaration) $1])

        ;; Fields
        (field-declaration
          [(modifiers type var-declarators semicolon)
           (make-object todo-node% $1 (build-src 1))]
          [(type var-declarators semicolon)
           (make-object todo-node% $1 (build-src 1))])

        (var-declarators
          [(var-declarator) (list $1)]
          [(var-declarators comma var-declarator) (cons $3 $1)])

        (var-declarator
          [(var-declarator-id) $1]
          [(var-declarator-id = var-initializer)
           (make-object todo-node% $1 (build-src 1))])

        (var-declarator-id 
          [(identifier)
           (make-object todo-node% $1 (build-src 1))]
          [(identifier dims)
           (make-object todo-node% $1 (build-src 1))])

        (var-initializer
          [(expr) $1]
          [(array-initializer) $1])

        ;; Methods
        (method-declaration
          [(method-header method-body) (make-object todo-node% $1 (build-src 1))])

        (method-header
          [(modifiers type method-declarator throws) (make-object todo-node% $1 (build-src 1))]
          [(modifiers void method-declarator throws)
           (make-object todo-node% $1 (build-src 1))]
          [(type method-declarator throws) (make-object todo-node% $1 (build-src 1))]
          [(void method-declarator throws)
           (make-object todo-node% $2 (build-src 2))])

        (method-declarator
          [(identifier l-paren formal-parameter-list r-paren) (list (make-object todo-node%
                                                                                 $1 (build-src 1)))]
          [(identifier l-paren r-paren) (list (make-object todo-node% $1 (build-src 1)))]
          [(identifier l-paren formal-parameter-list r-paren dims) (list (make-object todo-node% $1 (build-src 1)))]
          [(identifier l-paren r-paren dims) (list (make-object todo-node% $1 (build-src 1)))])

        (formal-parameter-list
          [(formal-parameter) (list $1)]
          [(formal-parameter-list comma formal-parameter) (cons $3 $1)])

        (formal-parameter
          [(type var-declarator-id) (make-object todo-node% $1 (build-src 1))]
          [(final type var-declarator-id) (make-object todo-node% $2 (build-src 2))])

        (throws-stmt
          [() null]
          [(throws class-type-list) $2])

        (class-type-list
          [(class-type) (list $1)]
          [(class-type-list comma class-type) (cons $3 $1)])

        (method-body
          [(block) $1]
          [(semicolon) #f])

        ;; Static Initializer
        (static-initializer
          [(static block) (make-object todo-node% $2 (build-src 2))]
          [(block) (make-object todo-node% $1 (build-src 1))])

        ;; Constructors 
        (constructor-declaration
          [(modifiers constructor-declarator throws constructor-body)
           (make-object todo-node% $1 (build-src 1))]
          [(constructor-declarator throws constructor-body)
           (make-object todo-node% $1 (build-src 1))])

        (constructor-declarator
          [(identifier l-paren formal-parameter-list r-paren) (list (make-object todo-node% $1 (build-src 1)))]
          [(identifier l-paren r-paren) (list (make-object todo-node% $1 (build-src 1)))])

        (constructor-body
          [(l-cbrack explicit-constructor-invocation block-stmts r-cbrack)
           (make-object todo-node% $2 (build-src 2))]
          [(l-cbrack explicit-constructor-invocation r-cbrack)
           (make-object todo-node% $2 (build-src 2))]
          [(l-cbrack block-stmts r-cbrack)
           (make-object todo-node% $2 (build-src 2))]
          [(l-cbrack r-cbrack)
           (make-object todo-node% null null)])

        (explicit-constructor-invocation
          [(this l-paren args r-paren semicolon)
           (make-object todo-node% $3 (build-src 1))]
          [(this l-paren r-paren semicolon)
           (make-object todo-node% null (build-src 1))]
          [(super l-paren args r-paren semicolon)
           (make-object todo-node% $3 (build-src 1))]
          [(super l-paren r-paren semicolon)
           (make-object todo-node% null (build-src 1))])

        ;; Interfaces
        (interface-declaration
          [(modifiers interface identifier extends-interfaces interface-body)
           (make-object todo-node% $1 (build-src 1))]
          [(modifiers interface identifier interface-body)
           (make-object todo-node% $1 (build-src 1))]
          [(interface identifier extends-interfaces interface-body)
           (make-object todo-node% $2 (build-src 2))]
          [(interface identifier interface-body)
           (make-object todo-node% $2 (build-src 2))])

        (extends-interfaces
          [(extends interface-type) (list $2)]
          [(extends-interfaces comma interface-type) (cons $3 $1)])

        (interface-body
          [(l-cbrack interface-member-declarations r-cbrack) $2])

        (interface-member-declarations
          [() null]

          [(interface-member-declarations interface-member-declaration) 
           (cond
             ((not $2) $1)
             ((list? $2) (append $2 $1))
             (else (cons $2 $1)))])

        (interface-member-declaration
          [(constant-declaration) $1]
          [(class-declaration) $1]
          [(interface-declaration) $1]
          [(abstract-method-declaration) $1]
          [(semicolon) #f])

        (constant-declaration
          [(field-declaration) $1])

        (abstract-method-declaration
          [(method-header semicolon) $1])

        ;; Array Initializer
        (array-initializer
          [(l-sbrack var-initializers comma r-sbrack)   (make-object todo-node% $2 (build-src 2))]
          [(l-sbrack var-initializers r-sbrack)         (make-object todo-node% $2 (build-src 2))]
          [(l-sbrack comma r-sbrack)                    (make-object todo-node%
                                                                     null (build-src 1))]
          [(l-sbrack r-sbrack)                          (make-object todo-node%
                                                                     null(build-src 1))])

        (var-initializers
          [(var-initializer)                        (list $1)]
          [(var-initializers comma var-initializer) (cons $3 $1)])

        ;; Blocks
        (block
          [(l-cbrack block-stmts r-cbrack)  (make-object todo-node% (reverse $2) )]
          [(l-cbrack r-cbrack)              (make-object todo-node% null )])

        (block-stmts
          [(block-stmt)             (if (list? $1) $1 (list $1))]
          [(block-stmts block-stmt) (if (list? $2) 
                                      (append (reverse $2) $1)
                                      (cons $2 $1))])

        (block-stmt
          [(class-declaration) $1]
          [(interface-declaration) $1]
          [(local-var-decl-stmt) $1]
          [(stmt) $1])

        (local-var-decl-stmt
          [(local-var-decl semicolon) $1]
          [(final local-var-decl semicolon) $2])

        (local-var-decl
          [(type var-declarators) (make-object todo-node% $1 (build-src 1))])

        (stmt
          [(stmt-no-trailing-substmt) $1]
          [(labeled-stmt) $1]
          [(if-then-stmt) $1]
          [(if-then-else-stmt) $1]
          [(while-stmt) $1]
          [(for-stmt) $1])

        (stmt-no-short-if
          [(stmt-no-trailing-substmt) $1]
          [(labeled-stmt-no-short-if) $1]
          [(if-then-else-stmt-no-short-if) $1]
          [(while-stmt-no-short-if) $1]
          [(for-stmt-no-short-if) $1])

        (stmt-no-trailing-substmt
          [(block) $1]
          [(empty-stmt) $1]
          [(expr-stmt) $1]
          [(switch-stmt) $1]
          [(do-stmt) $1]
          [(break-stmt) $1]
          [(continue-stmt) $1]
          [(return-stmt) $1]
          [(synchronized-stmt) $1]
          [(throw-stmt) $1]
          [(try-stmt) $1])

        (empty-stmt
          [(semicolon) (make-object todo-node% null)])

        (labeled-stmt
          [(identifier : stmt) (make-object todo-node% $1 (build-src 1))])

        (labeled-stmt-no-short-if
          [(identifier : stmt-no-short-if) (make-object todo-node% $1 (build-src 1))])

        (expr-stmt
          [(stmt-expr  semicolon) $1])

        (stmt-expr
          [(assignment) $1]
          [(pre-inc-expr) $1]
          [(pre-dec-expr) $1]
          [(post-inc-expr) $1]
          [(post-dec-expr) $1]
          [(method-invocation) $1]
          [(class-instance-creation-expr) $1])

        (if-then-stmt
          [(if l-paren expr r-paren stmt) 
           (make-object todo-node% $3 (build-src 3))])

        (if-then-else-stmt
          [(if l-paren expr r-paren stmt-no-short-if else stmt)
           (make-object todo-node% $3 (build-src 3))])

        (if-then-else-stmt-no-short-if
          [(if l-paren expr r-paren stmt-no-short-if else stmt-no-short-if)
           (make-object todo-node% $3 (build-src 3))])

        (switch-stmt
          [(switch l-paren expr r-paren switch-block)
           (make-object todo-node% $3 (build-src 3))])

        (switch-block
          [(l-cbrack switch-block-stmt-groups switch-labels r-cbrack)
           (make-object todo-node% $2 (build-src 2))]
          [(l-cbrack switch-block-stmt-groups r-cbrack) 
           (make-object todo-node% $2 (build-src 2))]
          [(l-cbrack switch-labels r-cbrack)
           (make-object todo-node% $2 (build-src 2))]
          [(l-cbrack r-cbrack) null])

        (switch-block-stmt-groups
          [(switch-block-stmt-group) (list $1)]
          [(switch-block-stmt-groups switch-block-stmt-group) (cons $2 $1)])

        (switch-block-stmt-group
          [(switch-labels block-stmts) (make-object todo-node% $2 (build-src 1))])

        (switch-labels
          [(switch-label) $1]
          [(switch-labels switch-label) (cons $2 $1)])

        (switch-label
          [(case constant-expr :) $2]
          [(default :) 'default])

        (while-stmt
          [(while l-paren expr r-paren stmt)
           (make-object todo-node% 'while (build-src 1))])

        (while-stmt-no-short-if
          [(while l-paren expr r-paren stmt-no-short-if)
           (make-object todo-node% 'while (build-src 1))])

        (do-stmt
          [(do stmt while l-paren expr r-paren semicolon)
           (make-object todo-node% 'do (build-src 1))])

        (for-stmt
          [(for l-paren for-init semicolon expr semicolon for-update r-paren
             stmt)
           (make-object todo-node% 'for (build-src 1))]
          [(for l-paren for-init semicolon semicolon for-update r-paren stmt)
           (make-object todo-node% 'for (build-src 1))])


        (for-stmt-no-short-if
          [(for l-paren for-init semicolon expr semicolon for-update r-paren stmt-no-short-if)
           (make-object todo-node% 'for (build-src 1))]
          [(for l-paren for-init semicolon semicolon for-update r-paren stmt-no-short-if)
           (make-object todo-node%  'for (build-src 1))])

        (for-init
          [() null]
          [(stmt-expr-list) (reverse $1)]
          [(local-var-decl) (reverse $1)])

        (for-update
          [() null]
          [(stmt-expr-list) (reverse $1)])

        (stmt-expr-list
          [(stmt-expr) (list $1)]
          [(stmt-expr-list comma stmt-expr) (cons $3 $1)])

        (break-stmt
          [(break identifier semicolon) (make-object todo-node% 'break (build-src 1))]
          [(break semicolon) (make-object todo-node% 'break (build-src 1))])

        (continue-stmt
          [(continue identifier semicolon) (make-object todo-node% 'continue (build-src 1))]
          [(continue semicolon) (make-object todo-node% 'continue (build-src 1))])

        (return-stmt
          [(return expr semicolon) (make-object todo-node% 'return (build-src 1))]
          [(return semicolon) (make-object todo-node% 'return (build-src 1))])

        (throw-stmt
          [(throw expr semicolon) (make-object todo-node% 'throw (build-src 1))])

        (synchronized-stmt
          [(synchronized l-paren expr r-paren block)
           (make-object todo-node% 'synchronized (build-src 1))])

        (try-stmt
          [(try block catches) (make-object todo-node% 'try (build-src 1))]
          [(try block catches finally-stmt) (make-object todo-node% 'try (build-src 1))]
          [(try block finally-stmt) (make-object todo-node% 'try (build-src 1))])

        (catches
          [(catch-clause) (list $1)]
          [(catches catch-clause) (cons $2 $1)])

        (catch-clause
          [(catch l-paren formal-parameter r-paren block)
           (make-object todo-node% 'catch (build-src 1))])

        (finally-stmt
          [(finally block) $2])

        (primary
          [(primary-no-new-array) $1]
          [(array-creation-expr) $1])

        (primary-no-new-array
          [(literal) $1]
          [(this) (make-object todo-node% 'this (build-src 1))]
          [(l-paren expr r-paren) $2]
          [(class-instance-creation-expr) $1]
          [(field-access) $1]
          [(method-invocation) $1]
          [(array-access) $1]
          [(primitive-type period class) (make-object todo-node% $1 (build-src 1))]
          [(name period class) (make-object todo-node% $1 (build-src 1))]
          [(void period class) (make-object todo-node% 'void (build-src 1))]
          [(name period this) (make-object todo-node% $1 (build-src 1))])

        (class-instance-creation-expr
          [(new class-or-interface-type l-paren args r-paren)
           (make-object todo-node% 'new (build-src 1))]
          [(new class-or-interface-type l-paren r-paren) 
           (make-object todo-node% 'new (build-src 1))]
          [(new class-or-interface-type l-paren args r-paren class-body)
           (make-object todo-node% 'new (build-src 1))]
          [(new class-or-interface-type l-paren r-paren class-body)
           (make-object todo-node% 'new (build-src 1))]
          [(primary period new identifier l-paren args r-paren class-body)
           (make-object todo-node% $1 (build-src 1))]
          [(primary period new identifier l-paren r-paren class-body)
           (make-object todo-node% $1 (build-src 1))]
          [(primary period new identifier l-paren args r-paren)
           (make-object todo-node% $1 (build-src 1))]
          [(primary period new identifier l-paren r-paren)
           (make-object todo-node% $1 (build-src 1))]
          [(name period new identifier l-paren args r-paren class-body)
           (make-object todo-node% $1 (build-src 1))]
          [(name period new identifier l-paren r-paren class-body)
           (make-object todo-node% $1 (build-src 1))]
          [(name period new identifier l-paren args r-paren)
           (make-object todo-node% $1 (build-src 1))]
          [(name period new identifier l-paren r-paren)
           (make-object todo-node% $1 (build-src 1))])

        (args
          [(expr) (list $1)]
          [(args comma expr) (cons $3 $1)])

        (array-creation-expr
          [(new primitive-type dim-exprs dims) (make-object todo-node% 'new (build-src 1))]
          [(new primitive-type dim-exprs) (make-object todo-node% 'new (build-src 1))]
          [(new class-or-interface-type dim-exprs dims)
           (make-object todo-node% 'new (build-src 1))]
          [(new class-or-interface-type dim-exprs)
           (make-object todo-node% 'new (build-src 1))]
          [(new primitive-type dims array-initializer) (make-object todo-node%
                                                                    'new (build-src 1))]
          [(new class-or-interface-type dims array-initializer) 
           (make-object todo-node% 'new (build-src 1))])

        (dim-exprs
          [(dim-expr) (list $1)]
          [(dim-exprs dim-expr) (cons $2 $1)])

        (dim-expr
          [(l-sbrack expr r-sbrack) $2])

        (field-access
          [(primary period identifier)    (make-object todo-node% $1 (build-src 1))]
          [(super period identifier)     (make-object todo-node% 'super (build-src 1))]
          [(name period super period identifier) (make-object todo-node% $1 (build-src 1))])

        (method-invocation
          [(name l-paren args r-paren) (make-object todo-node% $1 (build-src 1))]
          [(name l-paren r-paren) (make-object todo-node% $1 (build-src 1))]
          [(primary period identifier l-paren args r-paren)
           (make-object todo-node% $1 (build-src 1))]
          [(primary period identifier l-paren r-paren)
           (make-object todo-node% $1 (build-src 1))]
          [(super period identifier l-paren args r-paren)
           (make-object todo-node% 'new (build-src 1))]
          [(super period identifier l-paren r-paren)
           (make-object todo-node% 'new (build-src 1))]
          [(name period super period identifier l-paren args r-paren)
           (make-object todo-node% $1 (build-src 1))]
          [(name period super period identifier l-paren r-paren) 
           (make-object todo-node% $1 (build-src 1))])

        (array-access
          [(name l-sbrack expr r-sbrack)
           (make-object todo-node% $1 (build-src 1))]
          [(primary-no-new-array l-sbrack expr r-sbrack)
           (make-object todo-node% $1 (build-src 1))])

        (postfix-expr
          [(primary) $1]
          [(name) $1]
          [(post-inc-expr) $1]
          [(post-dec-expr) $1])

        (post-inc-expr
          [(postfix-expr ++) (make-object todo-node% $1 (build-src 1))])

        (post-dec-expr
          [(postfix-expr --) (make-object todo-node% $1 (build-src 1))])

        (unary-expr
          [(pre-inc-expr) $1]
          [(pre-dec-expr) $1]
          [(+ unary-expr) (make-object todo-node% '+ (build-src 1))]
          [(- unary-expr) (make-object todo-node% '- (build-src 1))]
          [(unary-expr-not-plus-minus) $1])

        (pre-inc-expr
          [(++ unary-expr) (make-object todo-node% '++  (build-src 1))])

        (pre-dec-expr
          [(-- unary-expr) (make-object todo-node% '-- (build-src 1))])

        (unary-expr-not-plus-minus
          [(postfix-expr) $1]
          [(~ unary-expr) (make-object todo-node% '~ (build-src 1))]
          [(! unary-expr) (make-object todo-node% '! (build-src 1))]
          [(cast-expr) $1])

        (cast-expr
          [(l-paren primitive-type dims r-paren unary-expr)
           (make-object todo-node% $2 (build-src 2))]
          [(l-paren primitive-type r-paren unary-expr)
           (make-object todo-node% $2 (build-src 2))]
          [(l-paren expr r-paren unary-expr-not-plus-minus)
           (make-object todo-node% $2 (build-src 2))]
          [(l-paren name dims r-paren unary-expr-not-plus-minus)
           (make-object todo-node% $2 (build-src 2))])

        (multiplicative-expr
          [(unary-expr) $1]
          [(multiplicative-expr * unary-expr)
           (make-object todo-node% $1 (build-src 1))]
          [(multiplicative-expr / unary-expr)
           (make-object todo-node% $1 (build-src 1))]
          [(multiplicative-expr % unary-expr)
           (make-object todo-node% $1 (build-src 1))])

        (additive-expr
          [(multiplicative-expr) $1]
          [(additive-expr + multiplicative-expr)
           (make-object todo-node% $1 (build-src 1))]
          [(additive-expr - multiplicative-expr)
           (make-object todo-node% $1 (build-src 1))])

        (shift-expr
          [(additive-expr) $1]
          [(shift-expr << additive-expr)
           (make-object todo-node% $1 (build-src 1))]
          [(shift-expr >> additive-expr)
           (make-object todo-node% $1 (build-src 1))]	
          [(shift-expr >>> additive-expr)
           (make-object todo-node% $1 (build-src 1))])


        (relational-expr
          [(shift-expr) $1]
          [(shift-expr < shift-expr)
           (make-object todo-node% $1 (build-src 1))]		
          [(relational-expr > shift-expr)
           (make-object todo-node% $1 (build-src 1))]	
          [(relational-expr <= shift-expr)
           (make-object todo-node% $1 (build-src 1))]	
          [(relational-expr >= shift-expr)
           (make-object todo-node% $1 (build-src 1))]	
          [(relational-expr instanceof reference-type)
           (make-object todo-node% $1 (build-src 1))])


        (equality-expr
          [(relational-expr) $1]
          [(equality-expr == relational-expr)
           (make-object todo-node% $1 (build-src 1))]	
          [(equality-expr != relational-expr)
           (make-object todo-node% $1 (build-src 1))])

        (and-expr
          [(equality-expr) $1]
          [(and-expr & equality-expr)
           (make-object todo-node% $1 (build-src 1))])


        (exclusive-or-expr
          [(and-expr) $1]
          [(exclusive-or-expr ^ and-expr)
           (make-object todo-node% $1 (build-src 1))])

        (inclusive-or-expr
          [(exclusive-or-expr) $1]
          [(inclusive-or-expr PIPE exclusive-or-expr)
           (make-object todo-node% $1 (build-src 1))])

        (conditional-and-expr
          [(inclusive-or-expr) $1]
          [(conditional-and-expr && inclusive-or-expr)
           (make-object todo-node% $1 (build-src 1))])

        (conditional-or-expr
          [(conditional-and-expr) $1]
          [(conditional-or-expr OR conditional-and-expr)
           (make-object todo-node% $1 (build-src 1))])

        (conditional-expr
          [(conditional-or-expr) $1]
          [(conditional-or-expr ? expr : conditional-expr)
           (make-object todo-node% $1 (build-src 1))])

        (assignment-expr
          [(assignment) $1])

        (assignment
          [(left-hand assignment-operator assignment-expr)
           (make-object todo-node% $1 (build-src 1))])      

        (left-hand
          [(name) $1]
          [(field-access) $1]
          [(array-access) $1])

        (assignment-operator
          [(=) '=]
          [(*=) '*=]
          [(/=) '/=]
          [(%=) '%=]
          [(+=) '+=]
          [(-=) '-=]
          [(<<=) '<<=]
          [(>>=) '>>=]
          [(>>>=) '>>>=]
          [(&=) '&=]
          [(^=) '^=]
          [(OREQUAL) 'or=])

        (expr
          [(assignment-expr) $1])

        (constant-expr
          [(expr) $1])))))
