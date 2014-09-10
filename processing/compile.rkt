(module compiler-processing racket
  (require "ast.rkt") 
  
  (provide compile-processing)

  (define (compile-processing ast)
    (cond ((not (null? ast)) (map (lambda (elem) (send elem ->racket)) ast))
          (else (list)))))

