(module ast-stmt racket 

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt"
           "../runtime.rkt")

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
  
  )
