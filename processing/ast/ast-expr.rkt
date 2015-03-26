#lang racket

(provide (all-defined-out))

(require racket/undefined
         "ast.rkt"
         "types.rkt"
         "errors.rkt"

         "../bindings.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST expression nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expression%
  (class ast-node%
         (inherit read-error)
         (field [type-info (create-type 'Object)])

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

         (inherit ->syntax-object set-scope! set-type! get-scope get-type)

         (define/override (->racket)
                          (->syntax-object
                            `(p-call ,@(node->racket primary)
                                     ,@(node->racket args))))

         (define/override (->type-check)
                          (node->type-check args)
                          (type-check-args (send primary get-id))
                          (node->type-check primary (get-type)))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings primary scope)
                          (node->bindings args scope))

         (define/override (->print)
                          `(method-call% ,(node->print primary)
                                         ,(node->print args)))

         (define (type-check-args id)
           (let* ([fn-bindings   (send (get-scope) get-functions id)]
                  [types         (map (lambda (x) (send x get-type)) args)]
                  [applicable-fn (applicable-function types fn-bindings)]
                  [promotable-fn (promotable-function types fn-bindings)])
             (cond
               [(and (null? applicable-fn) (null? promotable-fn))
                (method-not-applicable this
                                       (send id get-id)
                                       (map (lambda (x) (send x get-type))
                                            types))]
               [(null? applicable-fn)
                (begin
                  (set-type! (send (car promotable-fn) get-return-type))
                  (build-mangled-id (car promotable-fn)))]
               [else
                 (begin
                   (set-type! (send (car applicable-fn) get-return-type))
                   (build-mangled-id (car applicable-fn)))])))

         (define (build-mangled-id binding)
           (send primary build-mangled-id binding))

         (super-instantiate ())))

(define primary%
  (class expression%
         (init-field primary id)
         (field [mangled-id null])

         (inherit ->syntax-object set-scope! set-type!)

         (define/public (is-method?) (null? (send id get-list)))
         (define/public (get-id) id)

         (define/override (->racket)
                          (if (null? primary)
                            (if (is-method?)
                              `(#:call ,mangled-id)
                              `(#:send ,(send id get-full-id)
                                ,mangled-id))
                            `(#:send ,(node->racket primary)
                              ,mangled-id)))

         (define/override (->type-check type)
                          (and (not (null? primary))
                               (node->type-check primary))
                          (node->type-check id type)
                          (set-type! type))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (and (not (null? primary))
                               (node->bindings primary scope))
                          (node->bindings id scope))

         (define/override (->print)
                          `(primary% ,(node->print primary)
                                     ,(node->print id)))

         (define/public (build-mangled-id binding)
                        (set! mangled-id (send binding get-mangled-id)))

         (super-instantiate ())))

(define identifier%
  (class expression%
         (init-field id-list identifier)

         (inherit ->syntax-object set-scope! set-type! get-type get-scope)

         (define/public (get-id)       (string->symbol identifier))
         (define/public (get-list)     (reverse id-list))

         (define/public (get-full-id)
                        (string->symbol
                          (build-full-id (get-list))))

         (define/public (identifier->symbol)
                        (string->symbol (string-append "" identifier)))

         (define/override (->racket)
                          (->syntax-object (identifier->symbol)))

         (define/override (->type-check [ret-type null])
                          (if (null? ret-type)
                            (set-type! (get-id-type))
                            (set-type! ret-type)))

         (define/override (->bindings scope)
                          (set-scope! scope))

         ;;; lookup the id in current scope a get the defined type
         (define/public (get-id-type)
                        (let ([variable (send (get-scope) get-variable this)])
                          (if (null? variable)
                            (binding-not-found this (get-id))
                            (send (car variable) get-type))))

         ;;; generates a syntax object with mangled info
         (define/public (mangled-id mangled-id)
                        (->syntax-object mangled-id))


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
                               `(p-name ,(node->racket name)
                                        ,(conversion-type))]
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
         (define (conversion-type)
           (let ([defined-id-type (send name get-id-type)]
                 [type (get-type)])
             (if (and (primitive-type? defined-id-type)
                      (primitive-type? type))
               (cond
                 [(and (send defined-id-type char-type?)
                       (send type long-or-int-type?))
                  '#:char->int]
                 [(and (send defined-id-type long-or-int-type?)
                       (send type char-type?))
                  '#:int->char]
                 [(and (send defined-id-type long-or-int-type?)
                       (send type float-or-double-type?))
                  '#:int->float]
                 [else '#:none])
               '#:none)))

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
           (let ([type (get-type)])
             (cond
               [(array-type? type) value]
               [(primitive-type? type)
                (case (send type get-type)
                  [(float double) (exact->inexact value)]
                  [(char)
                   (if (send literal-type char-type?)
                     value
                     (integer->char value))]
                  [(int long)
                   (if (send literal-type char-type?)
                     (char->integer value)
                     value)]
                  [(boolean short byte) value]
                  [(Object String color) value] ;maybe obj and str ref?
                  [(null) 'null] ;maybe refecence type?
                  [else (type-error "Unknown type!")])]
               [(reference-type? type)
                (type-error "Reference-type not done")]
               [else (type-error "Not know type for literal")])))

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
                          `(assignment% ,operator
                                        ,(node->print left-val)
                                        ,(node->print right-val)))

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
                          `(left-value% ,(node->print value) ,left-value-type))
         (super-instantiate ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define array-id%
  (class expression%
         (init-field id dims)

         (inherit ->syntax-object set-scope! get-type)

         (define/public (get-id) (send id get-id))
         (define/public (get-array-id) id)
         (define/public (get-dims) dims)

         (define/override (->racket)
                          (->syntax-object (get-id)))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))


         (define/override (->print)
                          `(array-id% ,(send id get-id) ,dims))

         (super-instantiate ())))

(define new-array%
  (class expression%
         (init-field type dim-expr dims initializer)

         (inherit ->syntax-object set-scope! set-type!)

         (define/override (->racket)
                          (->syntax-object
                            (if (not (null? initializer))
                              `(p-vector ,@(node->racket initializer))
                              `(p-vector ,(node->racket dim-expr)
                                         ,(initial-value)))))

         (define/override (->type-check)
                          (check-dims)
                          (check-initializer)
                          (set-type! (create-type type
                                                  (+ (length dim-expr)
                                                     dims))))

         (define/override (->bindings scope)
                          (node->bindings dim-expr scope)
                          (node->bindings initializer scope)
                          (set-scope! scope))

         (define (initial-value)
           (cond
             [(primitive-type? type)
              (cond
                [(send type char-type?) #\space]
                [(send type integral-type?)   0]
                [(send type boolean-type?)   #f]
                [(send type numeric-type?)  0.0]
                [else undefined])]
             [else        undefined]))

         (define (check-dims)
           (node->type-check dim-expr)
           (map (lambda (x)
                  (unless (send (send x get-type) integral-type?)
                    (boolean-conversion-error x (send x get-type))))
                dim-expr))

         (define  (check-initializer)
           (unless (null? initializer)
             (node->type-check initializer)
             (map (lambda (x)
                    (let ([expr-type (send x get-type)])
                      (if (primitive-promotable?  type expr-type)
                        (send x set-type! type)
                        (type-conversion-error x expr-type type))))
                  initializer)))


         (define/override (->print)
                          `(new-array% ,(send type get-type)
                                       ,(node->print dim-expr)
                                       ,dims
                                       ,(node->print initializer)))

         (super-instantiate ())))

(define array-acces%
  (class expression%
         (init-field id expr)

         (define/public (get-id)   (node->racket id))
         (define/public (get-expr) (node->racket expr))

         (inherit ->syntax-object set-scope! set-type!)

         (define/override (->racket)
                          (->syntax-object
                            `(vector-ref ,(node->racket id)
                                         ,(node->racket expr))))

         (define/override (->type-check)
                          (node->type-check id)
                          (set-type! (send (send id get-type) get-type))
                          (check-expr))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings id scope)
                          (node->bindings expr scope))

         (define (check-expr)
           (node->type-check expr)
           (unless (send (send expr get-type) integral-type?)
             (boolean-conversion-error expr (send expr get-type))))

         (define/override (->print)
                          `(array-acces% ,(node->print id) ,(node->print expr)))
         (super-instantiate ())))
