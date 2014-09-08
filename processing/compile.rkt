(module compiler-processing racket
  (require "ast.rkt") 
  
  (provide compile-processing)

  (define (compile-processing ast)
    (cond ((not (null? ast)) (list (send ast to-racket 0)))
          (else (list)))))

