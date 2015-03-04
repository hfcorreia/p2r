#lang racket

(require rackunit
         "compile.rkt"
         rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (test-parser path)
  (test-case
    "Testing Parser")
  (check-not-exn
    (lambda () (build-ast path))))

(define (test-bindings path)
  (let ([ast (build-ast path)])
    (test-case
      "Checking Bindings"
      (check-not-exn
        (lambda () (bindings-check ast))))))

(define (test-types path)
  (let ([ast (build-ast path)])
    (test-case
      "Checking Types"
      (check-not-exn
        (lambda () (type-check ast))))))

(define (test-compilation path)
  (let ([ast (build-ast path)])
    (test-case
      "Compiling Code"
      (check-not-exn
        (lambda () (compile-processing ast))))))

(define (test-runtime path)
  (test-case
    "Requiring module"
    (check-not-exn
      (lambda () (system (string-append "racket " (path->string path)))))))

(define (full-tests path)
  (test-suite
    (format "Testing ~a" path)
    (test-parser      path)
    (test-bindings    path)
    (test-types       path)
    (test-compilation path)
    ;(test-runtime     path)
  ))

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

(current-directory "test")

(run-tests
  (make-test-suite
    "Processing Tests"
    (for/list ([dir (find-dirs (current-directory))])
              (make-test-suite
                (format "Testing dir: ~a" dir)
                (for/list ([file (find-processing-files dir)])
                          (full-tests file)))))
  'normal)
