#lang s-exp syntax/module-reader
processing/lang/processing

#:read processing-read
#:read-syntax processing-read-syntax
#:language-info '#(processing/lang/language-info get-language-info #f)
#:whole-body-readers? #t

(require syntax/strip-context
         racket/class
         "../compile.rkt"
         "../processing/runtime-bindings.rkt")

(provide processing-read
         processing-read-syntax
         processing-read-syntax-repl)

(define (processing-read input-port)
  (map syntax->datum (processing-read-syntax #f input-port)))

(define scope (new-scope))

(define (processing-read-syntax src input-port)
  (define compiled
    (compile-processing (build-ast src input-port) scope))
  (map strip-context compiled))

(define (processing-read-syntax-repl src input-port)
  (let* ([ast  (build-repl-interaction src input-port)]
         [code (map strip-context (compile-processing-repl ast scope))])
    (if (null? code)
      eof
      #`(begin #,@code))))
