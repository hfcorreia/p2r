(module compile racket

  (provide compile-processing
           build-ast)

  (require racket/file
           "ast/ast.rkt"
           "parser.rkt"
           "mode.rkt")
  ;;;
  (define (build-ast file #:input-port [input-port #f])
    (if (eq? input-port #f)
      (with-input-from-file file
                            (lambda ()
                              (parse-processing file (current-input-port)))
                            #:mode 'text)
      (parse-processing file input-port)))


  ;;;
  (define (compile-processing ast)
    (if (active-mode?)
        (append 
          (map (lambda (node) (send node ->racket)) ast)
          (list (send (make-object initializer% null) ->racket)))
        (map (lambda (node) (send node ->racket)) ast)))
  )
