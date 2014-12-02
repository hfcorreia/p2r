(module ast/expr racket

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt"
           "../lib/runtime.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST expression nodes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define expression%
    (class ast-node%
           (inherit read-error)

           (define/override (->racket)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define method-call% 
    (class expression%
           (init-field name args)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (let ((full-name (send name get-full-id)))
                              (->syntax-object
                                (cond 
                                  [(and (not full-name) (null? args))
                                   `(p-call ,(node->racket name))]
                                  [(and (not full-name) (not (null? args)))
                                   `(p-call ,(node->racket name) ,@(node->racket args))]
                                  [(and full-name (null? args))
                                   `(p-send  ,full-name 
                                            ,(node->racket name))]
                                  [else 
                                    `(p-send ,full-name 
                                             ,(node->racket name)
                                             ,@(node->racket args))]))))

           (super-instantiate ())))

  (define identifier%
    (class expression%
           (init-field id-list identifier)

           (inherit ->syntax-object)

           (define/public (get-id)   (string->symbol identifier))
           (define/public (get-list) (reverse id-list))

           (define/public (get-full-id) 
                          (if (null? id-list)
                            #f
                            (string->symbol 
                              (build-full-id (reverse id-list)))))

           (define/override (->racket)
                            (->syntax-object (identifier->symbol)))

           (define (identifier->symbol)
             (string->symbol (string-append "" identifier)))

           (define (build-full-id lst)
             (cond 
               [(null? lst) ""]
               [(eq? (length lst) 1) (format "~a" (car lst))]
               [else (format "~a-~a" (car lst) (build-full-id (cdr lst)))]))          

           (super-instantiate ())))

  (define literal%
    (class expression%
           (init-field value type)

           (inherit ->syntax-object)

           (define/override (->racket) 
                            (->syntax-object value))

           ;; TODO: use this to convert the literals 
           ;(define literal-types 
           ; (case type
           ;   [(float? double?)
           ;    (if (inexact? value) value (exact->inexact value))]
           ;   [(char? int? long? boolean? short? byte?) value]))

           (super-instantiate ())))

  (define arguments%
    (class expression%
           (init-field args)

           (inherit ->syntax-object)

           (define/public (get-args) args)

           (define/override (->racket)
                            (->syntax-object
                              (node->racket (reverse args))))

           (super-instantiate ())))

  (define binary-op%
    (class expression%
           (init-field operator arg1 arg2)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object 
                              `(,p-operator ,(node->racket arg1) 
                                            ,(node->racket arg2))))

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

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object 
                              `(,p-operator ,(node->racket arg))))

           (define p-operator
             (case operator
               ['+ 'p-pos]
               ['- 'p-neg]
               ['not 'p-bit-not]
               ['! 'p-not]))

           (super-instantiate ())))

  (define assignment%
    (class expression%
           (init-field operator left-val right-val) 

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object 
                              (if (equal? operator '=)
                                `(p-assignment ,(node->racket left-val)
                                               ,(node->racket right-val))
                                `(p-assignment ,assignment-operator
                                               ,(node->racket left-val)
                                               ,(node->racket right-val)))))

           (define assignment-operator
             (case operator
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

  (define new-array%
    (class expression%
           (init-field type dim-expr dims initializer)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object 
                              (if (not (null? initializer)) 
                                (node->racket initializer)
                                `(p-vector ,(node->racket dim-expr)
                                           ,(initial-value)))))

           (define (initial-value)
             (case type
               ['int        0]
               ['float      0.0]
               ['boolean    #f]
               ['char       #\space]
               [else        null]))

           (super-instantiate ())))

  (define array-dim%
    (class expression%
           (init-field expr)

           (inherit ->syntax-object)
           
           (define/override (->racket)
                            (->syntax-object
                              (node->racket expr)))

           (super-instantiate ())))

  (define array-acces%
    (class expression%
           (init-field id expr)

           (inherit ->syntax-object)
           
           (define/override (->racket)
                            (->syntax-object
                              `(vector-ref ,(node->racket id) 
                                           ,(node->racket expr))))

           (super-instantiate ())))

  (define array-initializer% 
    (class expression%
           (init-field initializers)

           (inherit ->syntax-object)
           
           (define/override (->racket)
                            (->syntax-object
                              `(vector ,@(node->racket initializers))))

           (super-instantiate ())))
  )
