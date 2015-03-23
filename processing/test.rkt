#lang racket

(require rackunit
         racket/runtime-path
         "compile.rkt"
         "processing/runtime-bindings.rkt"
         rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (test-parser path)
  (test-case
    "Testing Parser"
    (check-not-exn
      (lambda () (build-ast path)))))

(define (test-bindings ast scope)
  (test-case
    "Checking Bindings"
    (check-not-exn
      (lambda () (bindings-check ast scope)))))

(define (test-types ast)
  (test-case
    "Checking Types"
    (check-not-exn
      (lambda () (type-check ast)))))

(define (test-compilation ast scope)
  (test-case
    "Compiling Code"
    (check-not-exn
      (lambda () (compile-processing ast scope)))))

(define (test-runtime path)
  (test-case
    "Requiring module"
    (check-not-exn
      (lambda () (system (string-append "racket " (path->string path)))))))

(define (full-tests path scope)
  (test-suite
    (format "Testing ~a" path)
    (test-parser      path)
    (let ([ast (build-ast path)])
      ;; (test-bindings    ast scope)
      ;; (test-types       ast)
      (test-compilation ast scope))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-processing-files directory-path)
  (let ((files '()))
    (for ([path (directory-list directory-path)])
      (when (regexp-match? #rx"[.]pde$" path)
        (set! files (cons (build-path directory-path path) files))))
    files))

(define (find-dirs directory-path)
  (let ((dirs '()))
    (for ([path (directory-list directory-path)])
      (let ([complete-path (build-path directory-path path)])
        (and (directory-exists? complete-path)
             (set! dirs (cons complete-path dirs)))))
    dirs))

(define-runtime-path test "test")

(run-tests
  (make-test-suite
    "Processing Tests"
    (for/list ([dir (find-dirs test)])
              (make-test-suite
                (format "Testing dir: ~a" dir)
                (for/list ([file (find-processing-files dir)])
                          (full-tests file (new-scope))))))
  'normal)
