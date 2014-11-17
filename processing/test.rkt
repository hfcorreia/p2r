(module test racket

  (require rackunit
           rackunit/text-ui
           "compile.rkt"
           "ast/ast.rkt")

  (current-directory "examples")

  (define test-files
    (let ((files '()))
      (for ([path (in-directory (current-directory))])
        (when (regexp-match? #rx"[.]pde$" path)
          (set! files (cons path files))))
      files))




  (run-tests
    (make-test-suite
      "Processing Tests"
      (for/list ([path test-files])
                (test-suite
                  "Test for correct compilation"
                  #:before (lambda () (clear-todo))
                  (test-case
                    "Test"
                    (check-pred 
                      list?
                      (compile-processing (build-ast path))
                      (format "Erro: at ~a" path))
                    (check-false 
                      (begin (compile-processing (build-ast path)) todo?) 
                      (format "Error: Incomplete AST ~a" path)))))))

  )
