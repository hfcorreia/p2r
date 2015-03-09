#lang racket

(provide (all-defined-out))

(require racket/undefined
         "ast.rkt"
         "types.rkt"
         "../bindings.rkt"
         "errors.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST expression nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expression%
  (class ast-node%
         (inherit read-error)
         (field [type-info (create-type null 'Object)])

         (define/public (get-type) type-info)

         (define/public (set-type! type)
                        (set! type-info type))

         (define/override (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

         (define/override (->type-check)
                          (read-error (format "Invalid use of ->type-check ~a" this)))

         (define/override (->bindings scope)
                          (read-error (format "Invalid use of ->bindings ~a" this)))

         ;; type-error: symbol? symbol? -> exe:fail:read
         ;; raises a exception to signal type errors
         (define/public (type-error msg)
                        (read-error msg))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define method-call%
  (class expression%
         (init-field primary args)

         (inherit ->syntax-object set-scope! set-type! get-scope)

         (define/override (->racket)
                          (->syntax-object
                            `(p-call ,@(node->racket primary)
                                     ,@(node->racket args))))

         (define/override (->type-check)
                          (node->type-check primary)
                          (set-type! (send primary get-type))
                          (node->type-check args)
                          (type-check-args (send primary get-id) args))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings primary scope)
                          (node->bindings args scope))

         (define/override (->print)
                          `(method-call% ,(node->print primary)
                                         ,(node->print args)))

         (define (type-check-args id args-node)
           (let ([binding (send (get-scope) get-binding (send id get-id))]
                 ; unpack each arg from their ast-node%
                 [args-types    (map (lambda (x) (send x get-type)) args)])
             (cond
               [(and (eq? (length args-types) (send binding get-arity))
                     (andmap (lambda (t1 t2)
                               (or (type=? t1 t2)
                                   (object-type? t1 t2)
                                   (widening-primitive-conversion? t1 t2)))
                             (send binding get-args)
                             args-types))
                (map (lambda (node type) (send node set-type! type))
                     args-node
                     (send binding get-args))]
               [else
                 (method-not-applicable this
                                        (send id get-id)
                                        (map (lambda (x) (send x get-type))
                                             (send binding get-args))
                                        (map (lambda (x) (send x get-type))
                                             args-types))])))


         (super-instantiate ())))

(define primary%
  (class expression%
         (init-field primary id)

         (inherit ->syntax-object set-scope! set-type!)

         (define/public (is-method?) (null? (send id get-list)))
         (define/public (get-id) id)

         (define/override (->racket)
                          (if (null? primary)
                            (if (is-method?)
                              `(#:call ,(node->racket id))
                              `(#:send ,(send id get-full-id)
                                ,(node->racket id)))
                            `(#:send ,(node->racket primary)
                              ,(node->racket id))))

         (define/override (->type-check)
                          (and (not (null? primary))
                               (node->type-check primary))
                          (node->type-check id)
                          (set-type! (send id get-type)))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (and (not (null? primary))
                               (node->bindings primary scope))
                          (node->bindings id scope))

         (define/override (->print)
                          `(primary% ,(node->print primary)
                                     ,(node->print id)))

         (super-instantiate ())))

(define identifier%
  (class expression%
         (init-field id-list identifier)

         (inherit ->syntax-object set-scope! set-type! get-type get-scope)

         (define/public (get-id)   (string->symbol identifier))
         (define/public (get-list) (reverse id-list))

         (define/public (get-full-id)
                        (string->symbol
                          (build-full-id (get-list))))

         (define/public (identifier->symbol)
                        (string->symbol (string-append "" identifier)))

         (define/override (->racket)
                          (->syntax-object (identifier->symbol)))

         (define/override (->type-check)
                          (set-type! (get-id-type)))


         (define/override (->bindings scope)
                          (set-scope! scope))

         ;;; lookup the id in current scope a get the defined type
         (define/public (get-id-type)
                        (let ([binding (send (get-scope) get-binding (get-id))])
                          (cond
                            [(is-a? binding variable-binding%)
                             (send binding get-type)]
                            [(is-a? binding function-binding%)
                             (send binding get-return-type)]
                            [else (binding-not-found this (get-id))])))


         ;; build-full-id : (listof string?) -> string?
         ;; Receives a list of strings corresponding to the full qualified
         ;; and builds a '-' seperated string
         ;; so for Foo.Bazz.bar -> Foo-Bazz-bar
         (define (build-full-id lst)
           (cond
             [(null? lst) ""]
             [(eq? (length lst) 1) (format "~a" (car lst))]
             [else (format "~a-~a" (car lst) (build-full-id (cdr lst)))]))

         (define/override (->print)
                          `(identifier% ,identifier))

         (super-instantiate ())))

(define name%
  (class expression%
         (init-field name)

         (inherit ->syntax-object set-scope! set-type! get-type)

         (define/public (get-name) name)

         (define/override (->racket)
                          (->syntax-object
                            (cond
                              [(null? (send name get-list))
                               `(p-build-identifier ,(build-id)
                                                    ,(node->racket name))]
                              ; call fields
                              [else
                                `(get-field ,(node->racket name)
                                            ,(send name get-full-id))])))

         (define/override (->type-check)
                          (node->type-check name)
                          (set-type! (send name get-type)))


         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings name scope))

         ;; transforms the itentifier's value to the corret type
         (define (build-id)
           (let ([defined-id-type (send name get-id-type)]
                 [type (get-type)])
             (cond
               [(and (send defined-id-type char-type?)
                     (send type long-or-int-type?))
                char->integer]
               [(and (send type char-type?)
                     (send defined-id-type long-or-int-type?))
                integer->char]
               [else identity])))

         (define/override (->print)
                          `(name% ,(node->print name)))
         (super-instantiate ())))


(define literal%
  (class expression%
         (init-field value literal-type)

         (inherit ->syntax-object set-scope! set-type! get-type type-error)

         (define/override (->racket)
                          (->syntax-object (build-literal)))

         (define/override (->type-check)
                          (set-type! literal-type))

         (define/override (->bindings scope)
                          (set-scope! scope))

         (define (build-literal)
           (let ([type (send (get-type) get-type)])
             (case type
               [(float double) (exact->inexact value)]
               [(char) (if (send literal-type char-type?) value (integer->char value))]
               [(int long) (if (send literal-type char-type?) (char->integer value) value)]
               [(boolean short byte) value]
               [(Object String color) value] ;maybe should not be here
               [(null) 'null]
               [else (type-error "Unknown type!")])))

         (define/override (->print)
                          `(literal% ,(send literal-type get-type) ,value))

         (super-instantiate ())))


(define binary-op%
  (class expression%
         (init-field op arg1 arg2)

         (inherit ->syntax-object set-scope! set-type!)

         (define/override (->racket)
                          (->syntax-object
                            `(,p-op ,(node->racket arg1)
                                    ,(node->racket arg2))))

         (define/override (->type-check)
                          (node->type-check arg1)
                          (node->type-check arg2)
                          (let* ([t1 (send arg1 get-type)]
                                 [t2 (send arg2 get-type)]
                                 [result (binary-op-type-check op t1 t2)])
                            (if (eq? 'error result)
                              (binary-error this op t1 t2)
                              (begin
                                (set-type! result)
                                (send arg1 set-type! result)
                                (send arg2 set-type! result)))))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings arg1 scope)
                          (node->bindings arg2 scope))

         (define p-op
           (case op
             ['+ 'p-add]
             ['- 'p-sub]
             ['* 'p-mul]
             ['/ 'p-div]
             ['% 'p-mod]
             ['& 'p-bit-and]
             ['^ 'p-bit-xor]
             ['< 'p-lt]
             ['> 'p-gt]
             ['! 'p-gt]
             ['== 'p-eq]
             ['!= 'p-not-eq]
             ['<= 'p-lt-eq]
             ['>= 'p-gt-eq]
             ['pipe 'p-bit-or]
             ['&& 'p-and]
             ['or 'p-or]
             ['<< 'p-shiftl]
             ['>> 'p-shiftr]
             ['>>> 'p-shiftr-zero]
             ['instanceof 'p-instanceof]))

         (define/override (->print)
                          `(binary-op% ,op ,(node->print arg1) ,(node->print arg2)))
         (super-instantiate ())))

(define unary-op%
  (class expression%
         (init-field op arg)

         (inherit ->syntax-object set-scope! set-type!)

         (define/override (->racket)
                          (->syntax-object
                            (case op
                              [(pre++ pre-- pos++ pos--)
                               `(,p-operator ,(node->racket (send arg get-name)))]
                              [else `(,p-operator ,(node->racket arg))])))

         (define/override (->type-check)
                          (node->type-check arg)
                          (let* ([t (send arg get-type)]
                                 [result (unary-op-type-check op t)])
                            (if (eq? 'error result)
                              (unary-error this op t)
                              (begin
                                (send arg set-type! result)
                                (set-type! result)))))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings arg scope))

         (define p-operator
           (case op
             ['+ 'p-pos]
             ['- 'p-neg]
             ['~ 'p-bit-not]
             ['! 'p-not]
             ['pre++ 'p-pre-inc]
             ['pre-- 'p-pre-dec]
             ['pos++ 'p-pos-inc]
             ['pos-- 'p-pos-dec]))

         (define/override (->print)
                          `(unary-op%))
         (super-instantiate ())))

(define assignment%
  (class expression%
         (init-field operator left-val right-val)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-assignment ,assignment-operator
                                           ,(node->racket left-val)
                                           ,(node->racket right-val))))

         (define/override (->type-check)
                          (node->type-check left-val)
                          (node->type-check right-val))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings left-val scope)
                          (node->bindings right-val scope))

         (define assignment-operator
           (case operator
             ['=    'p-assign]
             ['*=   'p-mul]
             ['/=   'p-div]
             ['%=   'p-mod]
             ['+=   'p-add]
             ['-=   'p-sub]
             ['&=   'p-bit-and]
             ['^=   'p-bit-xor]
             ['<<=  'p-shiftl]
             ['>>=  'p-shiftr]
             ['>>>= 'p-shiftr-zero]
             ['or=  'p-bit-or]))

         (define/override (->print)
                          `(assignment%))
         (super-instantiate ())))

(define left-value%
  (class expression%
         (init-field value left-value-type)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-left-value ,@(generate))))

         (define/override (->type-check)
                          (node->type-check value))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings value scope))

         (define (check-type)
           (set! left-value-type
             (case left-value-type
               ['name (if (null? (send value get-list))
                        'name
                        'qual-name)]
               [else left-value-type])))

         (define (generate)
           (check-type)
           (case left-value-type
             ['name       (list (node->racket value) '#:name)]
             ['qual-name  (list (send value get-id)
                                (send value get-full-id)
                                '#:name)]
             ['field      (list (send value get-id)
                                (send value get-primary)
                                '#:field)]
             ['array      (list (send value get-id)
                                (send value get-expr)
                                '#:array)]))

         (define/override (->print)
                          `(left-value%))
         (super-instantiate ())))

(define new-array%
  (class expression%
         (init-field type dim-expr dims initializer)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            (if (not (null? initializer))
                              (node->racket initializer)
                              `(p-vector ,(node->racket dim-expr)
                                         ,(initial-value)))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (define (initial-value)
           (case type
             ['int        0]
             ['float      0.0]
             ['boolean    #f]
             ['char       #\space]
             [else        undefined]))

         (define/override (->print)
                          `(new-array%))

         (super-instantiate ())))

(define array-dim%
  (class expression%
         (init-field expr)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            (node->racket expr)))

         (define/override (->type-check)
                          (node->type-check expr))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings expr scope))

         (define/override (->print)
                          `(array-dim%))
         (super-instantiate ())))

(define array-acces%
  (class expression%
         (init-field id expr)

         (define/public (get-id)   (node->racket id))
         (define/public (get-expr) (node->racket expr))

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(vector-ref ,(node->racket id)
                                         ,(node->racket expr))))

         (define/override (->type-check)
                          (node->type-check id)
                          (node->type-check expr))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings id scope)
                          (node->bindings expr scope))

         (define/override (->print)
                          `(array-acces%))
         (super-instantiate ())))

(define array-initializer%
  (class expression%
         (init-field initializers)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(vector ,@(node->racket initializers))))

         (define/override (->type-check)
                          (node->type-check initializers))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings initializers scope))

         (define/override (->print)
                          `(array-initializer%))

         (super-instantiate ())))
