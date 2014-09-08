#lang s-exp syntax/module-reader
processing/lang/processing

#:read processing-read
#:read-syntax processing-read-syntax
#:whole-body-readers? #t

(require syntax/strip-context
         "../parser.rkt"
         "../compile.rkt")

(define (processing-read in)
  (map syntax->datum (processing-read-syntax #f in)))

(define (processing-read-syntax src in)
  (define ast (parse-processing src in))
  (define compiled-ast (compile-processing ast))
  (let ((stripped (map strip-context compiled-ast)))
    (begin
      ;(fprintf (current-output-port) (car value))
      stripped)))


