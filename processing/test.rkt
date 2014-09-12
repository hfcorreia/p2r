(module test racket

  (require rackunit
           rackunit/text-ui
           "compile.rkt"
           "ast.rkt")

  (current-directory "examples")

  (run-tests
    (test-suite
      "Processing Tests"
      #:before (lambda () (clear-todo))
      (for ([path (in-directory (current-directory))])
        (when (regexp-match? #rx"[.]pde$" path)
          (test-case
            "Compiler checks"
            (check-pred list? 
                        (compile-processing (build-ast path))
                        (format "Error: at ~a" path))
            (check-false 
              (begin
                (compile-processing (build-ast path))
                todo?)
              (format "Error: Incomplete AST ~a" path))))))
    'normal))
