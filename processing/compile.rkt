(module compile racket
  (require 
    racket/file
    "ast.rkt"
    "parser.rkt") 

  (provide compile-processing)

  (define (ast->xml ast path)
    (with-output-to-file path
                         (lambda ()
                           (printf (send ast ->xml)))
                         #:exists 'replace))

  (define (compile-processing src input-port)
    (let ((ast (parse-processing src input-port)))
      (cond ((not (null? ast)) (map (lambda (elem) (send elem ->racket)) ast))
            (else (list))))))
