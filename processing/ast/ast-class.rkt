(module ast/class racket 

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt"
           "../lib/runtime.rkt")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST class nodes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define class-stmt%
    (class ast-node%
           (inherit read-error)

           (define/override (->racket)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define class-node%
    (class class-stmt%
           (init-field name body)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object
                              `(p-class ,(node->racket name) ,@(node->racket body))))

           (super-instantiate ())))

  (define new-node%
    (class class-stmt%
           (init-field name)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object
                              `(make-object ,(node->racket name))))

           (super-instantiate ())))

  (define class-field%
    (class class-stmt%
           (init-field modifiers type vars)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object
                              `(p-class-field ,@(node->racket vars))))

         ;[(public)         'public]
         ;[(protected)      'protected]
         ;[(private)        'private]
         ;[(abstract)       'abstract]
         ;[(final)          'final]
         ;[(native)         'native]
         ;[(static)         'static]
         ;[(synchronized)   'synchronized]
         ;[(transient)      'transient]
         ;[(volatile)       'volatile]

        (super-instantiate ())))
    )
