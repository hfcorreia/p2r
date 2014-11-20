(module ast/class racket 

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt"
           "../lib/runtime.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST class nodes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define class-node%
    (class ast-node%
           (init-field name body)

           (inherit read-error)
           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              `(p-class ,(node->racket name) ,@(node->racket body))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))

  (define new-node%
    (class ast-node%
           (init-field name)

           (inherit read-error)
           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              `(make-object ,(node->racket name))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))
  )
