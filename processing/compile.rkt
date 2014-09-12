(module compile racket
  (require 
    racket/file
    "ast.rkt"
    "parser.rkt") 

  (provide ast->xml
           compile-processing-from-file
           compile-processing-from-port)

  (define (ast->xml ast path)
    (with-output-to-file path
                         (lambda ()
                           (printf (send ast ->xml 0)))
                         #:exists 'replace))

  (define (compile-processing-from-file file)
    (with-input-from-file file
                          (lambda ()
                            (parse-processing file (current-input-port)))
                          #:mode 'text))

  (define (compile-processing-from-port src input-port)
    (let ((ast (parse-processing src input-port)))
      (cond ((not (null? ast)) (map (lambda (elem) (send elem ->racket)) ast))
            (else (list))))))
