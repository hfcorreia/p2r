#lang racket/base

(provide (all-defined-out))

(require racket/class
         racket/undefined
         "ast.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST expression nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define expression%
  (class ast-node%
         (inherit read-error)
         (field [type-info undefined])

         (define/public (get-type) type-info)

         (define/public (set-type! type) (set! type-info type))

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

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-call ,@(node->racket primary) 
                                     ,@(node->racket args))))

         (define/override (->type-check) 
                          (node->type-check primary)
                          (node->type-check args))

         (define/override (->bindings scope) 
                          (set-scope! scope))
                        
         (super-instantiate ())))

(define primary%
  (class expression%
         (init-field primary id)

         (inherit ->syntax-object set-scope!)

         (define/public (is-method?) (null? (send id get-list)))

         (define/override (->racket) 
                          (if (null? primary)
                            (if (is-method?)
                              `(#:call ,(node->racket id))
                              `(#:send ,(send id get-full-id) 
                                ,(node->racket id)))
                            `(#:send ,(node->racket primary) 
                              ,(node->racket id))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope) 
                          (set-scope! scope))

         (super-instantiate ())))

(define identifier%
  (class expression%
         (init-field id-list identifier)

         (inherit ->syntax-object set-scope!)

         (define/public (get-id)   (string->symbol identifier))
         (define/public (get-list) (reverse id-list))

         (define/public (get-full-id) 
                        (string->symbol 
                          (build-full-id (get-list))))

         (define/public (identifier->symbol)
                        (string->symbol (string-append "" identifier)))

         (define/override (->racket)
                          (->syntax-object (identifier->symbol)))

         (define/override (->type-check) #t)

         (define/override (->bindings scope) 
                          (set-scope! scope))
                        
         ;; build-full-id : (listof string?) -> string?
         ;; Receives a list of strings corresponding to the full qualified
         ;; and builds a '-' seperated string
         ;; so for Foo.Bazz.bar -> Foo-Bazz-bar
         (define (build-full-id lst)
           (cond 
             [(null? lst) ""]
             [(eq? (length lst) 1) (format "~a" (car lst))]
             [else (format "~a-~a" (car lst) (build-full-id (cdr lst)))]))          

         (super-instantiate ())))

(define name%
  (class expression% 
         (init-field name)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object 
                            (cond
                              [(null? (send name get-list)) 
                               (node->racket name)]
                              [(eq? 'length (send name get-id)) 
                               `(p-array-length ,(send name get-full-id)
                                                ,(node->racket name))]
                              [else 
                                `(get-field ,(node->racket name)
                                            ,(send name get-full-id))])))

         (define/override (->type-check) #t)

         (define/override (->bindings scope) 
                          (set-scope! scope))
                        
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
           (case type
             [(float double) (exact->inexact value)]
             [(undef char int long boolean short byte) value]
             [(String color) value] ;maybe should not be here
             [(null) 'null]
             [else (type-error "Unknown type!")])))

         (super-instantiate ())))


(define binary-op%
  (class expression%
         (init-field operator arg1 arg2)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object 
                            `(,p-operator ,(node->racket arg1) 
                                          ,(node->racket arg2))))

         (define/override (->type-check)
                          (node->type-check arg1)
                          (node->type-check arg2))
         
         (define/override (->bindings scope) 
                          (set-scope! scope))

         (define p-operator
           (case operator
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

         (super-instantiate ())))

(define unary-op%
  (class expression%
         (init-field operator arg)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object 
                            `(,p-operator ,(node->racket arg))))

         (define/override (->type-check)
                          (node->type-check arg))

        (define/override (->bindings scope)
                         (set-scope! scope))
         (define p-operator
           (case operator
             ['+ 'p-pos]
             ['- 'p-neg]
             ['not 'p-bit-not]
             ['! 'p-not]
             ['pre++ 'p-pre-inc]
             ['pre-- 'p-pre-dec]
             ['pos++ 'p-pos-inc]
             ['pos-- 'p-pos-dec]))

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
                          (set-scope! scope))
                        
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

         (super-instantiate ())))

(define left-value%
  (class expression%
         (init-field value type)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object 
                            `(p-left-value ,@generate ,key-type)))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (define (check-type)
           (set! type
             (case type
               ['name (if (null? (send value get-list))
                        'name
                        'qual-name)]
               [else type])))


         (define key-type 
           (begin 
             (check-type)
             (case type
               ['qual-name  '#:qual-name]
               ['name       '#:name]
               ['field      '#:field]
               ['array      '#:array])))

         (define generate
           (begin 
             (check-type)
             (case type
               ['name       (list (node->racket value))]
               ['qual-name  (list (send value get-id) (send value get-full-id))]
               ['field      (list (send value get-id) (send value get-primary))]
               ['array      (list (send value get-id) (send value get-expr))])))

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

                        
         (super-instantiate ())))

(define array-dim%
  (class expression%
         (init-field expr)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            (node->racket expr)))

         (define/override (->type-check) #t)

         (define/override (->bindings scope) 
                          (set-scope! scope))

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

         (define/override (->type-check) #t)

         (define/override (->bindings scope) 
                          (set-scope! scope))

         (super-instantiate ())))

(define array-initializer% 
  (class expression%
         (init-field initializers)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(vector ,@(node->racket initializers))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope) 
                          (set-scope! scope))

         (super-instantiate ())))
