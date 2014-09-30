(module ast-expr racket

  (require racket/class
           "ast.rkt"
           "ast-utils.rkt"
           "../lib/runtime.rkt")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST expression nodes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define expression%
    (class ast-node%
           (inherit read-error)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Method Call
  (define method-call% 
    (class expression%
           ;; inits
           (init-field name args)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-name) name)
           (define/public (get-args) args)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              (if (null? args)
                                `(p-call ,(node->racket name))
                                `(p-call ,(node->racket name)
                                           ,@(node->racket args)))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<method-call>~a~a~%~a</method-call>"
                                    (make-string indent #\space)
                                    (send name ->xml (+ indent 2))
                                    (if (null? args) 
                                      ""
                                      (send args ->xml (+ indent 2)))
                                    (make-string (+ 2 indent) #\space)))

           (super-instantiate ())))

  ;;; Identifier 
  (define identifier%
    (class expression%
           ;; inits
           (init-field identifier)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-id) identifier)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object (identifier->symbol)))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<identifier id=\"~a\" />"
                                    (make-string indent #\space)
                                    identifier))

           ;; Aux functions
           (define (identifier->symbol)
             (string->symbol 
               (string-append "p-" identifier)))

           (super-instantiate ())))

  ;;; Literals
  (define literal%
    (class expression%
           ;; inits
           (init-field value type)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-value) value)
           (define/public (get-type) type)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket) (->syntax-object value))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<literal type=\"~a\" value=\"~a\" />"
                                    (make-string indent #\space)
                                    type
                                    value))

           (super-instantiate ())))

  ;;; Arguments List
  (define arguments%
    (class expression%
           ;; inits
           (init-field args)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-args) args)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              (node->racket args)))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<args>~a~%~a</args>"
                                    (make-string indent #\space)
                                    (string-append*
                                      (map (lambda (arg) 
                                             (send arg ->xml (+ indent 2))) 
                                           args))
                                    (make-string indent #\space)))
           (super-instantiate ())))

  ;;; Binary Operator
  (define binary-op%
    (class expression%
           ;; inits
           (init-field operator arg1 arg2)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(,p-operator ,(node->racket arg1) 
                                            ,(node->racket arg2))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<binary-op id=\"~a\">~a~a~%~a</binary-op>"
                                    (make-string indent #\space)
                                    operator
                                    (send arg1 ->xml (+ indent 2))
                                    (send arg2 ->xml (+ indent 2))
                                    (make-string indent #\space)))
           ;; Aux function
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

  ;;; Unary Operator
  (define unary-op%
    (class expression%
           ;; inits
           (init-field operator arg)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(,p-operator ,(node->racket arg))))


           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<unary-op id=\"~a\">~a~%~a</unary-op>"
                                    (make-string indent #\space)
                                    operator
                                    (send arg ->xml (+ indent 2))
                                    (make-string indent #\space)))
           ;; Aux function
           (define p-operator
             (case operator
               ['+ 'p-pos]
               ['- 'p-neg]
               ['not 'p-bit-not]
               ['! 'p-not]))

           (super-instantiate ())))

  ;;; assignment
  (define assignment%
    (class expression%
           ;; inits
           (init-field operator left-val right-val) 

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              (if (equal? operator '=)
                                `(p-assignment ,(node->racket left-val)
                                               ,(node->racket right-val))
                                `(p-assignment ,assignment-operator
                                               ,(node->racket left-val)
                                               ,(node->racket right-val)))))


           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<assignment>~a~a~%~a</assignment>"
                                    (make-string indent #\space)
                                    (send left-val ->xml (+ indent 2))
                                    (send right-val ->xml (+ indent 2))
                                    (make-string indent #\space)))
           ;; aux function
           (define assignment-operator
             (case operator
               ['*= 'p-mul]
               ['/= 'p-div]
               ['%= 'p-mod]
               ['+= 'p-add]
               ['-= 'p-sub]
               ['&= 'p-bit-and]
               ['^= 'p-bit-xor]
               ['<<= 'p-shiftl]
               ['>>= 'p-shiftr]
               ['>>>= 'p-shiftr-zero]
               ['or= 'p-bit-or]))

           (super-instantiate ())))

  )
