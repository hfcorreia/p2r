#lang s-exp syntax/module-reader
processing/lang/processing

#:read processing-read
#:read-syntax processing-read-syntax
#:whole-body-readers? #t

(require syntax/strip-context
         "../parser.rkt")

(define (processing-read in)
  (map syntax->datum (processing-read-syntax #f in)))
  
(define (processing-read-syntax src in)
  (list (strip-context (parse-processing src in))))
    

