(module compiler-processing racket
  (require "ast.rkt") 
  
  (provide compile-processing)

  (define (compile-processing ast-list)
    (map compile-processing-ast ast-list))

  (define (compile-processing-ast ast)
    (send ast to-racket)))
