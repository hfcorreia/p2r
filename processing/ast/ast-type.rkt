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

           (define/override (->racket)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Type nodes
  (define primitive-type% 
    (class type%
           (inherit-field type)

           (define/override (->racket)
                            (send type ->racket))

           (super-instantiate ())))
  )
