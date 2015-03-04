#lang racket

(provide parse-processing)

(require parser-tools/yacc
         parser-tools/lex
         syntax/readerr
         racket/undefined

         "todo.rkt"
         "mode.rkt"

         "lexer.rkt"
         "ast/ast.rkt"
         "ast/types.rkt"
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
         (make-object literal% $1 (make-object primitive-type% 'int) (build-src 1))]
        [(long-lit)
         (make-object literal% $1 (make-object primitive-type% 'long) (build-src 1))]
        [(float-lit)
         (make-object literal% $1 (make-object primitive-type% 'float) (build-src 1))]
        [(double-lit)
         (make-object literal% $1 (make-object primitive-type% 'double) (build-src 1))]
        [(boolean-lit)
         (make-object literal% $1 (make-object primitive-type% 'boolean) (build-src 1))]
        [(string-lit)
         (make-object literal% $1 (make-object reference-type% null 'String) (build-src 1))]
        [(char-lit)
         (make-object literal% $1 (make-object primitive-type% 'char) (build-src 1))]
        [(null-lit)
         (make-object literal% null (make-object primitive-type% 'null) (build-src 1))]
        [(color-lit)
         (make-object literal% $1 (make-object primitive-type% 'color) (build-src 1))])

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Types
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (<type>
        [(<primitive-type>) $1]
        [(<reference-type>) $1])

      (<primitive-type>
        [(<numeric-type>) $1]
        [(color)   (make-object primitive-type% 'color)]
        [(boolean) (make-object primitive-type% 'boolean)])

      (<numeric-type>
        [(<integral-type>) $1]
        [(<floating-type>) $1])

      (<integral-type>
        [(byte)   (make-object primitive-type% 'byte)]
        [(short)  (make-object primitive-type% 'short)]
        [(int)    (make-object primitive-type% 'int)]
        [(long)   (make-object primitive-type% 'long)]
        [(char)   (make-object primitive-type% 'char)])

      (<floating-type>
        [(float)  (make-object primitive-type% 'float)]
        [(double) (make-object primitive-type% 'double)])

      (<void>
        [(void)     (make-object primitive-type% 'void)])

      (<reference-type>
        [(<name>)
         (make-object reference-type% (send $1 get-list) (send $1 get-id))]
        [(<array-type>) $1])

      (<array-type>
        [(<primitive-type> <dims>)
         (make-object array-type% $2 null $1)]
        [(<name> <dims>)
         (make-object array-type% $2 (send $1 get-list) (send $1 get-id))])

      (<class-or-interface-type>
        [(<name>) $1])


      (<class-type>
        [(<class-or-interface-type>) $1])

      (<interface-type>
        [(<class-or-interface-type>) $1])

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Dims & Args
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (<args>
        [(<expr>) (list $1)]
        [(<args> comma <expr>)  (cons $3 $1)])

      (<dim-exprs>
        [(<dim-expr>)             (list $1)]
        [(<dim-exprs> <dim-expr>) (cons $2 $1)])

      (<dim-expr>
        [(l-sbrack <expr> r-sbrack)
         (make-object array-dim% $2 (build-src 2))])

      (<dims>
        ;; returns a number corresponding to the dimension of the array
        [(l-sbrack r-sbrack)                1]
        [(<dims> l-sbrack r-sbrack) (add1 $1)])
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
        [() (make-object compilation-unit% null)]
        [(<import-declarations> <global-declarations>)
         (make-object compilation-unit%
                      (append (reverse $1) (reverse $2))
                      (build-src 1 2))]
        [(<global-declarations>)
         (make-object compilation-unit%
                      (reverse $1)
                      (build-src 1))])
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
         (make-todo 'import (build-src 2))])

      (<type-import-on-demand-declaration>
        [(import <name> period * semicolon)
         (make-todo 'import (build-src 2))])

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
         (make-todo 'class-decl (build-src 1))]
        [(<modifiers> class identifier <super> <class-body>)
         (make-todo 'class-decl (build-src 1))]
        [(<modifiers> class identifier <interfaces> <class-body>)
         (make-todo 'class-decl (build-src 1))]
        [(<modifiers> class identifier <class-body>)
         (make-todo 'class-decl (build-src 1))]
        [(class identifier <super> <interfaces> <class-body>)
         (make-todo 'class-decl (build-src 1))]
        [(class identifier <super> <class-body>)
         (make-todo 'class-decl (build-src 1))]
        [(class identifier <interfaces> <class-body>)
         (make-todo 'class-decl (build-src 1))]
        [(class identifier <class-body>)
         (make-object class-node%
                      (make-object identifier% null $2 (build-src 2))
                      $3
                      (build-src 1 3))])

      (<super>
        [(extends <class-type>)
         (make-todo 'super-class-type (build-src 1))])

      (<interfaces>
        [(implements <interface-type-list>)
         (make-todo 'implments-types (build-src 1))])

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
           (set-active-mode! #t)
           (make-object function-decl%
                        (first $1)    ; mods
                        (second $1)   ; ret-type
                        (third $1)    ; id
                        (fourth $1)   ; parameter-list
                        (fifth $1)    ; throws
                        $2            ; body
                        (build-src 1 2)))])

      (<method-declaration>
        [(<method-header> <method-body>)
         (begin
           ; Changes from static mode to active mode: see mode.rkt
           (set-active-mode! #t)
           (make-object method-decl%
                        (first $1)    ; mods
                        (second $1)   ; ret-type
                        (third $1)    ; id
                        (fourth $1)   ; parameter-list
                        (fifth $1)    ; throws
                        $2            ; body
                        (build-src 1 2)))])

      ;; (mod ret-type id parameter-list throws)
      (<method-header>
        [(<modifiers> <type> <method-declarator> <throws>)
         (list $1 $2 (car $3) (cdr $3) $4)]
        [(<modifiers> <type> <method-declarator>)
         (list $1 $2 (car $3) (cdr $3) null)]
        [(<type> <method-declarator> <throws>)
         (list null $1 (car $2) (cdr $2) $3)]
        [(<type> <method-declarator>)
         (list null $1 (car $2) (cdr $2) null)]
        [(<modifiers> <void> <method-declarator> <throws>)
         (list $1 $2 (car $3) (cdr $3) $4)]
        [(<modifiers> <void> <method-declarator>)
         (list $1 $2 (car $3) (cdr $3) null)]
        [(<void> <method-declarator> <throws>)
         (list null $1 (car $2) (cdr $2) $3)]
        [(<void> <method-declarator>)
         (list null $1 (car $2) (cdr $2) null)])

      (<method-declarator>
        [(identifier l-paren <formal-parameter-list> r-paren)
         (cons (make-object identifier% null $1 (build-src 1)) (reverse $3))]
        [(identifier l-paren r-paren)
         (cons (make-object identifier% null $1 (build-src 1)) null)]
        [(identifier l-paren <formal-parameter-list> r-paren <dims>)
         (make-todo 'method-declarator (build-src 2))]
        [(identifier l-paren r-paren <dims>)
         (make-todo 'method-declarator (build-src 2))])

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
         (make-todo 'throws (build-src 2))])

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
         (make-todo 'construct-decl (build-src 1))]
        [(<modifiers> <constructor-declarator> <constructor-body>)
         (make-todo 'construct-decl (build-src 1))]
        [(<constructor-declarator> <throws> <constructor-body>)
         (make-todo 'construct-decl (build-src 1))]
        [(<constructor-declarator> <constructor-body>)
         (make-todo 'construct-decl (build-src 1))])

      (<constructor-declarator>
        [(identifier l-paren <formal-parameter-list> r-paren)
         (make-todo 'construct-declarator (build-src 1))]
        [(identifier l-paren r-paren)
         (make-todo 'construct-declarator (build-src 1))])

      (<constructor-body>
        [(l-cbrack <explicit-constructor-invocation> <block-stmts> r-cbrack)
         (make-todo 'construct-body (build-src 2))]
        [(l-cbrack <explicit-constructor-invocation> r-cbrack)
         (make-todo 'construct-body (build-src 2))]
        [(l-cbrack <block-stmts> r-cbrack)
         (make-todo 'construct-body (build-src 2))]
        [(l-cbrack r-cbrack)
         (make-todo 'construct-body (build-src 2))])

      (<explicit-constructor-invocation>
        [(this l-paren <args> r-paren semicolon)
         (make-todo 'constrct-invocation (build-src 1))]
        [(this l-paren r-paren semicolon)
         (make-todo 'constrct-invocation (build-src 1))]
        [(super l-paren <args> r-paren semicolon)
         (make-todo 'constrct-super-invocation (build-src 1))]
        [(super l-paren r-paren semicolon)
         (make-todo 'constrct-super-invocation (build-src 1))])
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Array init
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (<array-initializer>
        [(l-cbrack <var-initializers> r-cbrack)
         (make-object array-initializer% (reverse $2) (build-src 1 3))]
        [(l-cbrack r-cbrack)
         (make-object array-initializer% null (build-src 1 2))])

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
         (make-object var-decl% $1 $2 (reverse $3)  (build-src 1 4))]
        [(<type> <var-declarators> semicolon)
         (make-object var-decl% null $1 (reverse $2)  (build-src 1 3))])

      (<field-declaration>
        [(<modifiers> <type> <var-declarators> semicolon)
         (make-object class-field% $1 $2 (reverse $3)  (build-src 1 4))]
        [(<type> <var-declarators> semicolon)
         (make-object class-field% null $1 (reverse $2)  (build-src 1 3))])

      (<var-declarators>
        [(<var-declarator>) (list $1)]
        [(<var-declarators> comma <var-declarator>) (cons $3 $1)])

      (<var-declarator>
        [(<var-decl-id>)  (list $1 (make-object literal% undefined 'undef (build-src 1)))]
        [(<var-decl-id> = <var-initializer>) (list $1 $3)])

      (<var-decl-id>
        [(identifier)
         (make-object identifier% null $1 (build-src 1))]
        [(identifier <dims>)
         (make-todo 'var-declartor-id (build-src 1))])

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
         (make-object var-decl% $1 $2 (reverse $3)  (build-src 1 3))]
        [(<type> <var-declarators>)
         (make-object var-decl% null $1 (reverse $2)  (build-src 1 2))])

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
         (make-todo 'labeld-stmt (build-src 1))])

      (<labeled-stmt-no-short-if>
        [(identifier : <stmt-no-short-if>)
         (make-todo 'labeld-stmt-no-if (build-src 1))])

      (<switch-stmt>
        [(switch l-paren <expr> r-paren <switch-block>)
         (make-todo 'switch-stmt (build-src 1))])

      (<switch-block>
        [(l-cbrack <switch-block-stmt-groups> <switch-labels> r-cbrack)
         (make-todo 'switch-block (build-src 2))]
        [(l-cbrack <switch-block-stmt-groups> r-cbrack)
         (make-todo 'switch-block (build-src 2))]
        [(l-cbrack <switch-labels> r-cbrack)
         (make-todo 'switch-block (build-src 2))]
        [(l-cbrack r-cbrack)
         (make-todo 'switch-block (build-src 2))])

      (<switch-block-stmt-groups>
        [(<switch-block-stmt-group>) (list $1)]
        [(<switch-block-stmt-groups> <switch-block-stmt-group>) (cons $2 $1)])

      (<switch-block-stmt-group>
        [(<switch-labels> <block-stmts>)
         (make-todo 'switch-block-stmt-group (build-src 1))])

      (<switch-labels>
        [(<switch-label>) $1]
        [(<switch-labels> <switch-label>) (cons $2 $1)])

      (<switch-label>
        [(case <constant-expr> :)
         (make-todo 'switch-case (build-src 1))]
        [(default :)
         (make-todo 'switch-default (build-src 1))])

      (<break-stmt>
        [(break identifier semicolon)
         (make-todo 'break (build-src 1))]
        [(break semicolon)
         (make-object break% (build-src 1))])

      (<continue-stmt>
        [(continue identifier semicolon)
         (make-todo 'continue (build-src 1))]
        [(continue semicolon)
         (make-object continue% (build-src 1))])

      (<return-stmt>
        [(return <expr> semicolon)
         (make-object return% $2 (build-src 1))]
        [(return semicolon)
         (make-object return% null (build-src 1))])

      (<synchronized-stmt>
        [(synchronized l-paren <expr> r-paren <block>)
         (make-todo 'synchronized (build-src 1))])

      (<throw-stmt>
        [(throw <expr> semicolon)
         (make-todo 'throw (build-src 1))])

      (<try-stmt>
        [(try <block> <catches>)
         (make-todo 'try (build-src 1))]
        [(try <block> <catches> <finally-stmt>)
         (make-todo 'try (build-src 1))]
        [(try <block> <finally-stmt>)
         (make-todo 'try (build-src 1))])

      (<catches>
        [(<catch-clause>) (list $1)]
        [(<catches> <catch-clause>) (cons $2 $1)])

      (<catch-clause>
        [(catch l-paren <formal-parameter> r-paren <block>)
         (make-todo 'catch (build-src 1))])

      (<finally-stmt>
        [(finally <block>)
         (make-todo 'finally-stmt (build-src 1))])

      (<primary>
        [(<primary-no-new-array>) $1]
        [(<array-creation-expr>) $1])

      (<primary-no-new-array>
        [(<literal>) $1]
        [(this) (make-object this-node% (build-src 1))]
        [(l-paren <expr> r-paren) $2]
        [(<class-instance-creation-expr>) $1]
        [(<field-access>) $1]
        [(<method-call>) $1]
        [(<color-instance-creation>) $1]
        [(<array-access>)  $1])

      (<field-access>
        [(<primary> period identifier)
         (make-object field-acces%
                      $1
                      (make-object identifier% null $3 (build-src 3))
                      (build-src 1 3))]
        [(super period identifier)
         (make-todo 'super-field-access (build-src 1))]
        [(<name> period super period identifier)
         (make-todo 'name-super-field-access (build-src 1))])

      (<array-access>
        [(<name> l-sbrack <expr> r-sbrack)
         (make-object array-acces% $1 $3 (build-src 1 4))]
        [(<primary-no-new-array> l-sbrack <expr> r-sbrack)
         (make-object array-acces% $1 $3 (build-src 1 4))])


      (<method-call>
        [(<data-conversion> l-paren <args> r-paren)
         (make-object method-call%
                      (make-object primary% null $1 (build-src 1))
                      (reverse $3)
                      (build-src 1 4))]
        [(<name> l-paren <args> r-paren)
         (make-object method-call%
                      (make-object primary% null $1 (build-src 1))
                      (reverse $3)
                      (build-src 1 4))]
        [(<name> l-paren r-paren)
         (make-object method-call%
                      (make-object primary% null $1 (build-src 1))
                      null
                      (build-src 1 3))]
        [(<primary> period identifier l-paren <args> r-paren)
         (make-object method-call%
                      (make-object primary%
                                   $1
                                   (make-object identifier% null $3 (build-src 3))
                                   (build-src 1 3))
                      (reverse $5)
                      (build-src 1 6))]
        [(<primary> period identifier l-paren r-paren)
         (make-object method-call%
                      (make-object primary%
                                   $1
                                   (make-object identifier% null $3 (build-src 3))
                                   (build-src 1 3))
                      null
                      (build-src 1 5))]
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        [(super period identifier l-paren <args> r-paren)
         (make-todo 'super-method-call (build-src 1))]
        [(super period identifier l-paren r-paren)
         (make-todo 'super-method-call (build-src 1))]
        [(<name> period super period identifier l-paren <args> r-paren)
         (make-todo 'method-call (build-src 1))]
        [(<name> period super period identifier l-paren r-paren)
         (make-todo 'method-call (build-src 1))])

      (<data-conversion>
        [(int)     (make-object identifier% null "int"     (build-src 1))]
        [(char)    (make-object identifier% null "char"    (build-src 1))]
        [(byte)    (make-object identifier% null "byte"    (build-src 1))]
        [(boolean) (make-object identifier% null "boolean" (build-src 1))]
        [(float)   (make-object identifier% null "float"   (build-src 1))])

      (<class-instance-creation-expr>
        [(new <class-or-interface-type> l-paren r-paren)
         (make-object new-node% $2 null (build-src 1 4))]
        [(new <class-or-interface-type> l-paren <args> r-paren)
         (make-object new-node% $2 (reverse $4) (build-src 1 5))]

        ;; TODO
        [(new <class-or-interface-type> l-paren <args> r-paren <class-body>)
         (make-todo 'new (build-src 1))]
        [(new <class-or-interface-type> l-paren r-paren <class-body>)
         (make-todo 'new (build-src 1))]


        [(<primary> period new identifier l-paren <args> r-paren <class-body>)
         (make-todo 'new (build-src 1))]
        [(<primary> period new identifier l-paren r-paren <class-body>)
         (make-todo 'new (build-src 1))]
        [(<primary> period new identifier l-paren <args> r-paren)
         (make-todo 'new (build-src 1))]
        [(<primary> period new identifier l-paren r-paren)
         (make-todo 'new (build-src 1))]

        [(<name> period new identifier l-paren <args> r-paren <class-body>)
         (make-todo 'new (build-src 1))]
        [(<name> period new identifier l-paren r-paren <class-body>)
         (make-todo 'new (build-src 1))]
        [(<name> period new identifier l-paren <args> r-paren)
         (make-todo 'new (build-src 1))]
        [(<name> period new identifier l-paren r-paren)
         (make-todo 'new (build-src 1))])

      (<color-instance-creation>
        [(color l-paren <args> r-paren)
         (make-object method-call%
                      (make-object primary%
                                   null
                                   (make-object identifier% null "color" (build-src 1))
                                   (build-src 1))
                      (reverse $3)
                      (build-src 1 4))])

      (<array-creation-expr>
        [(new <primitive-type> <dim-exprs>)
         (make-object new-array% $2 (reverse $3) null null (build-src 1 3))]
        [(new <primitive-type> <dim-exprs> <dims>)
         (make-object new-array% $2 (reverse $3) $4 null (build-src 1 4))]
        [(new <primitive-type> <dims> <array-initializer>)
         (make-object new-array% $2 null $3 $4 (build-src 1 4))]
        [(new <class-or-interface-type> <dim-exprs>)
         (make-object new-array% $2 $3 null null (build-src 1 3))]
        [(new <class-or-interface-type> <dim-exprs> <dims>)
         (make-object new-array% $2 $3 $4 null (build-src 1 4))]
        [(new <class-or-interface-type> <dims> <array-initializer>)
         (make-object new-array% $2 null $3 $4 (build-src 1 4))])

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Expressions and Assignments
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (<assignment>
        [(<left-hand> <assignment-operator> <assignment-expr>)
         (make-object assignment% $2 $1 $3 (build-src 1))])

      (<left-hand>
        [(<name>)
         (make-object left-value% $1 'name (build-src 1))]
        [(<field-access>)
         (make-object left-value% $1 'field (build-src 1))]
        [(<array-access>)
         (make-object left-value% $1 'array (build-src 1))])

      (<postfix-expr>
        [(<primary>) $1]
        [(<name>)         (make-object name% $1 (build-src 1))]
        [(<post-inc-expr>) $1]
        [(<post-dec-expr>) $1])

      (<post-inc-expr>
        [(<postfix-expr> ++)
         (make-object unary-op% 'pos++ $1 (build-src 1 2))])

      (<post-dec-expr>
        [(<postfix-expr> --)
         (make-object  unary-op% 'pos-- $1 (build-src 1 2))])

      (<unary-expr>
        [(<pre-inc-expr>) $1]
        [(<pre-dec-expr>) $1]
        [(+ <unary-expr>)
         (make-object unary-op% '+ $2  (build-src 1 2))]
        [(- <unary-expr>)
         (make-object unary-op% '- $2  (build-src 1 2))]
        [(<unary-expr-not-plus-minus>) $1])

      (<pre-inc-expr>
        [(++ <unary-expr>)
         (make-object unary-op% 'pre++ $2  (build-src 1 2))])

      (<pre-dec-expr>
        [(-- <unary-expr>)
         (make-object unary-op%'pre-- $2 (build-src 1 2))])

      (<unary-expr-not-plus-minus>
        [(<postfix-expr>) $1]
        [(~ <unary-expr>)
         (make-object unary-op% '~ $2  (build-src 2))]
        [(! <unary-expr>)
         (make-object unary-op% '! $2  (build-src 2))]
        [(<cast-expr>) $1])

      (<cast-expr>
        [(l-paren <primitive-type> <dims> r-paren <unary-expr>)
         (make-todo 'cast-expr (build-src 2))]
        [(l-paren <primitive-type> r-paren <unary-expr>)
         (make-todo 'cast-expr (build-src 2))]
        [(l-paren <expr> r-paren <unary-expr-not-plus-minus>)
         (make-todo 'cast-expr (build-src 2))]
        [(l-paren <name> <dims> r-paren <unary-expr-not-plus-minus>)
         (make-todo 'cast-expr (build-src 2))])

      (<multiplicative-expr>
        [(<unary-expr>) $1]
        [(<multiplicative-expr> * <unary-expr>)
         (make-object binary-op% '* $1 $3 (build-src 1 3))]
        [(<multiplicative-expr> / <unary-expr>)
         (make-object binary-op% '/ $1 $3 (build-src 1 3))]
        [(<multiplicative-expr> % <unary-expr>)
         (make-object binary-op% '% $1 $3 (build-src 1 3))])

      (<additive-expr>
        [(<multiplicative-expr>) $1]
        [(<additive-expr> + <multiplicative-expr>)
         (make-object binary-op% '+ $1 $3 (build-src 1 3))]
        [(<additive-expr> - <multiplicative-expr>)
         (make-object binary-op% '- $1 $3 (build-src 1 3))])

      (<shift-expr>
        [(<additive-expr>) $1]
        [(<shift-expr> << <additive-expr>)
         (make-object binary-op% '<< $1 $3 (build-src 1 3))]
        [(<shift-expr> >> <additive-expr>)
         (make-object binary-op% '>> $1 $3 (build-src 1 3))]
        [(<shift-expr> >>> <additive-expr>)
         (make-object binary-op% '>>> $1 $3 (build-src 1 3))])

      (<relational-expr>
        [(<shift-expr>) $1]
        [(<shift-expr> < <shift-expr>)
         (make-object binary-op% '< $1 $3 (build-src 1 3))]
        [(<relational-expr> > <shift-expr>)
         (make-object binary-op% '> $1 $3 (build-src 1 3))]
        [(<relational-expr> <= <shift-expr>)
         (make-object binary-op% '<= $1 $3 (build-src 1 3))]
        [(<relational-expr> >= <shift-expr>)
         (make-object binary-op% '>= $1 $3 (build-src 1 3))]
        [(<relational-expr> instanceof <reference-type>)
         (make-object binary-op% 'instanceof $1 $3 (build-src 1 3))])

      (<equality-expr>
        [(<relational-expr>) $1]
        [(<equality-expr> == <relational-expr>)
         (make-object binary-op% '== $1 $3 (build-src 1 3))]
        [(<equality-expr> != <relational-expr>)
         (make-object binary-op% '!= $1 $3 (build-src 1 3))])

      (<and-expr>
        [(<equality-expr>) $1]
        [(<and-expr> & <equality-expr>)
         (make-object binary-op% '& $1 $3 (build-src 1 3))])

      (<exclusive-or-expr>
        [(<and-expr>) $1]
        [(<exclusive-or-expr> ^ <and-expr>)
         (make-object binary-op% '^ $1 $3 (build-src 1 3))])

      (<inclusive-or-expr>
        [(<exclusive-or-expr>) $1]
        [(<inclusive-or-expr> PIPE <exclusive-or-expr>)
         (make-object binary-op% 'pipe $1 $3 (build-src 1 3))])

      (<conditional-and-expr>
        [(<inclusive-or-expr>) $1]
        [(<conditional-and-expr> && <inclusive-or-expr>)
         (make-object binary-op% '&& $1 $3 (build-src 1 3))])

      (<conditional-or-expr>
        [(<conditional-and-expr>) $1]
        [(<conditional-or-expr> OR <conditional-and-expr>)
         (make-object binary-op% 'or $1 $3 (build-src 1 3))])

      (<conditional-expr>
        [(<conditional-or-expr>) $1]
        [(<conditional-or-expr> ? <expr> : <conditional-expr>)
         (make-todo 'ternary-op (build-src 1 3))])

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
        [(<expr>) $1]))))
