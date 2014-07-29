(module processing-ast racket

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST struct definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; All nodes inherit from ast-node%
  (define ast-node%
    (class object%
           (init-field src-info)
           (abstract to-racket)
           (super-new)))

  (define todo-node%
    (class ast-node%
           (init-field msg)
           (define/override (to-racket)
                            (format "Todo: ~a" msg))
           (super-new)))
  )

