(module ast-stmt racket 

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt"
           "../runtime.rkt")

  ;;; global stmt
  (define global-stmt%
    (class ast-node% 
           (init-field stmt)

           ;; getters
           (define/public (get-stmt) stmt)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (send stmt ->racket))

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
  )
