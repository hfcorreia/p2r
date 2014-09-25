(module ast-stmt racket 

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt"
           "../runtime.rkt")

  ;;; global stmt
  (define global-stmt%
    (class ast-node% 
           (init-field stmt)

           (inherit-field src-info)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              `(begin 
                                 (void ,(send stmt ->racket)))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~a<global-stmt>~a~%~a</global-stmt>~%"
                                    (make-string indent #\space)
                                    (send stmt ->xml (+ indent 2))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  ;;; global declaration
  (define global-decl%
    (class ast-node% 
           (init-field decl)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (send decl ->racket))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~a<global-decl>~a~%~a</global-decl>~%"
                                    (make-string indent #\space)
                                    (send decl ->xml (+ indent 2))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  ;;; variable declaration
  (define vars-decl%
    (class ast-node% 
           ;; TODO: types and modifiers
           (init-field modifiers type vars)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              `(begin ,@(map (lambda(var)
                                               (send var ->racket))
                                             vars))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<vars-decl>~a~a~%~a~%~a</vars-decl>"
                                    (make-string indent #\space)
                                    (if (null? modifiers)
                                      ""
                                      (send modifiers ->xml (+ indent 2)))
                                    (send type ->xml (+ indent 2))
                                    (string-append*
                                      (map (lambda (var)
                                             (send var ->xml (+ indent 2)))
                                           vars))
                                    (make-string indent #\space)))
           (super-instantiate ())))

  ;;; variable declaration
  (define var-decl%
    (class ast-node% 
           ;; TODO: types and modifiers
           (init-field var value)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              (if (null? value)
                                `(p-declaration ,(send var ->racket))
                                `(p-declaration ,(send var ->racket)
                                                ,(send value ->racket)))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<var-decl>~a~a~%~a</var-decl>"
                                    (make-string indent #\space)
                                    (send var ->xml (+ indent 2))
                                    (if (not (null? value))
                                      (send value ->xml (+ indent 2))
                                      "") 
                                    (make-string indent #\space)))
           (super-instantiate ())))

  (define block%
    (class ast-node% 
           (init-field stmts)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              (if (null? stmts) 
                                `(let () (void))
                                `(let ()
                                   ,@(map (lambda (stmt)
                                            (send stmt ->racket))
                                          stmts)))))
                                   

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<block>~a~%~a</block>~%"
                                    (make-string indent #\space)
                                    (string-append*
                                      (map (lambda (stmt)
                                             (send stmt ->xml (+ indent 2)))
                                           stmts))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  (define method-decl% 
    (class ast-node% 
           (init-field header body)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(define 
                                 (,(send (send header get-id) ->racket)
                                   ,@(send header ->racket))
                                 ,(send body ->racket))))


           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<method-decl>~a~%~a~%~a</method-decl>"
                                    (make-string indent #\space)
                                    (send header ->xml (+ indent 2))
                                    (send body ->xml (+ indent 2))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  (define method-header%
    (class ast-node% 
           (init-field modifiers type id parameters throws)

           (inherit ->syntax-object)

           ;; Getters
           (define/public (get-id) id)
           (define/public (get-parameters) parameters)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              (map (lambda(arg)
                                     (send arg ->racket))
                                   parameters)))



           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<method-decl>~a~%~a~%~a~%~a~%~a~%~a</method-decl>"
                                    (make-string indent #\space)
                                    (if (null? modifiers)
                                      ""
                                      (send modifiers ->xml (+ indent 2)))
                                    (send type ->xml (+ indent 2))
                                    (send id ->xml (+ indent 2))
                                    (if (null? parameters)
                                      ""
                                      (string-append*
                                        (map (lambda (var)
                                               (send var ->xml (+ indent 2)))
                                             parameters)))
                                    (if (null? throws)
                                      ""
                                      (send throws ->xml (+ indent 2)))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  (define formal-parameter% 
    (class ast-node% 
           (init-field final type id)

           (inherit ->syntax-object)

           ;; Getters
           (define/public (get-id) id)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              (send id ->racket)))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<formal-parameter>~a~%~a~%~a~%~a<formal-parameter/>"
                              (make-string indent #\space)
                              final
                              (send type ->xml (+ indent 2))
                              (send id ->xml (+ indent 2))
                              (make-string indent #\space)))

           (super-instantiate ())))

  (define return% 
    (class ast-node% 
           (init-field expr)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              (if(null? expr)
                                (void)
                                (send expr ->racket))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<return>~%~a~%~a<return/>"
                              (make-string indent #\space)
                              (if (null? expr)
                                ""
                                (send expr ->xml (+ indent 2)))
                              (make-string indent #\space)))

           (super-instantiate ())))
  )
