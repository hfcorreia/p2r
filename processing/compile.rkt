(module compile racket

  (provide ast->xml
           compile-processing
           build-ast)

  (require racket/file
           "ast/ast.rkt"
           "parser.rkt") 
  ;;;
  (define (ast->xml ast #:file [path #f])
    (if (path-string? path)
      (with-output-to-file path
                           (lambda ()
                             (map (lambda (elem) 
                                    (printf (send elem ->xml 0))) ast))
                           #:exists 'replace)
      (map (lambda (elem) (printf (send elem ->xml 0))) ast)))


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
    (list (send ast ->racket))))
