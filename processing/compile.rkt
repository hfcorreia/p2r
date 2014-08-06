(module compiler-processing racket
  (require "ast.rkt") 
  
  (provide compile-processing)

  (define (compile-processing ast)
    (list (send ast to-racket 0))))

