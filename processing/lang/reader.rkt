#lang s-exp syntax/module-reader
processing/lang/processing

#:read processing-read
#:read-syntax processing-read-syntax
#:whole-body-readers? #t

(require syntax/strip-context
         "../compile.rkt")

(define (processing-read input-port)
  (map syntax->datum (processing-read-syntax #f input-port)))

(define (processing-read-syntax src input-port)
  (map strip-context (compile-processing (build-ast src #:input-port input-port))))

(define (processing-read-syntax-repl src input-port)
  (let ([code (processing-read-syntax src input-port)])
    (if (null? code)
      eof
      #`(begin #,@code))))
