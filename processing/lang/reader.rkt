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
  (map strip-context (compile-processing-from-port src input-port)))



