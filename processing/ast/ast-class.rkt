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

           (super-instantiate ())))

  (define method-decl% 
    (class class-stmt%
           (init-field header body)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object 
                              `(define/public ,(node->racket header)
                                              (call/ec (lambda (return)
                                                         ,(node->racket body))))))

           (super-instantiate ())))

  (define method-header%
    (class class-stmt%
           (init-field modifiers type id parameters throws)

           (inherit ->syntax-object)

           (define/public (get-id) id)
           (define/public (get-parameters) parameters)

           (define/override (->racket)
                            (->syntax-object 
                              `(,(node->racket id)
                                 ,@(node->racket (reverse parameters)))))

           (super-instantiate ())))

  (define formal-parameter% 
    (class class-stmt%
           (init-field final type id)

           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object 
                              (node->racket id)))

           (super-instantiate ())))

  (define this-node%
    (class class-stmt%
           (inherit ->syntax-object)

           (define/override (->racket)
                            (->syntax-object 
                              this))


           (super-instantiate ())))
  )
