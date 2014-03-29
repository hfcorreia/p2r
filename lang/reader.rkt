#lang s-exp syntax/module-reader
p2r/lang/processing

#:read processing-read
#:read-syntax processing-read-syntax
#:whole-body-readers? #t

(require syntax/strip-context
         "../parser.rkt")

(define (processing-read in)
  (processing-read-syntax #f in))

(define (processing-read-syntax src in)
  (strip-context  (parse-processing src in)))

