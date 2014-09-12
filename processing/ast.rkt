(module ast racket

  (require racket/class
           syntax/readerr
           "runtime.rkt")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST struct definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; All nodes inherit from ast-node%
  (define ast-node%
    (class object%
           ;; inits
           (init-field src-info)

           ;; getters
           (define/public (get-src-info) src-info)

           ;; read-err: string? -> exn:fail:read
           ;; raises an exception with source of the expression
           (define/public (read-error msg)
                          (apply raise-read-error (cons msg src-info)))


           ;; ->s : datum? -> syntax-object?
           ;; converts the datum to a syntax object using stored src-info
           (define/public (->syntax-object datum)
                          (datum->syntax #'test
                                         datum
                                         src-info
                                         (read-syntax #f 
                                                      (open-input-string "orig")) 
                                         ))

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/public (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/public (->xml indent)
                          (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Global stmt
  (define global-stmt%
    (class ast-node% 
           (init-field stmt)

           ;; getters
           (define/public (get-stmt) stmt)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (send stmt ->racket))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~a<global-stmt>~a~%~a</global-stmt>~%"
                                    (make-string indent #\space)
                                    (send stmt ->xml (+ indent 2))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  ;;; Identifier 
  (define identifier%
    (class ast-node%
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
    (class ast-node%
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

  ;;; Method Call
  (define method-call% 
    (class ast-node%
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
                                         ,(send args ->racket))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<method-call>~a~a~%~a</method-call>"
                                    (make-string indent #\space)
                                    (send name ->xml (+ indent 2))
                                    (send args ->xml (+ indent 2))
                                    (make-string (+ 2 indent) #\space)))
           (super-instantiate ())))

  ;;; Arguments List
  (define arguments%
    (class ast-node%
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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Debug stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define todo-node%
    (class ast-node%
           (init-field child msg)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object
                              (println "")))

           (define/override (->xml indent)
                            (format "~a<todo msg=\"~a\">~%~a~%~a</todo>~%"
                                    (make-string indent #\space)
                                    msg
                                    (send child ->xml (+ indent 2))
                                    (make-string indent #\space)))
           (super-new)))

  )
