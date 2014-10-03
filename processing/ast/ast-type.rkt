(module ast/type racket

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST types nodes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define type% 
    (class ast-node%
           (init-field type)

           (inherit read-error)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (read-error (format "Invalid use of ->xml ~a" this)))

           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Type nodes
  (define primitive-type% 
    (class type%
           (inherit-field type)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (send type ->racket))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<primitive-type type=\"~a\" />"
                                    (make-string indent #\space)
                                    type))
           (super-instantiate ())))
  )
