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
    (map (lambda (node) (send node ->racket)) ast))
  )
