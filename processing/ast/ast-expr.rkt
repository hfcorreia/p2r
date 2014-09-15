(module ast-expr racket

  (require racket/class
           "ast.rkt"
           "../runtime.rkt")

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
                              `(dispatch ,(send name ->racket) 
                                         ,@(send args ->racket))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<method-call>~a~a~%~a</method-call>"
                                    (make-string indent #\space)
                                    (send name ->xml (+ indent 2))
                                    (send args ->xml (+ indent 2))
                                    (make-string (+ 2 indent) #\space)))
           (super-instantiate ())))

  ;;; Identifier 
  (define identifier%
    (class expression%
           ;; inits
           (init-field identifier)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-identifier) identifier)

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
             (if (symbol? identifier)
               identifier 
               (string->symbol identifier))) 

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
                              (map (lambda (arg)
                                     (send arg ->racket))
                                   args)))

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

  )
