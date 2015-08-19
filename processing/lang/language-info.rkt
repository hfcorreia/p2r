#lang racket/base

(provide get-language-info)

(require "../lexer.rkt")

(define (get-language-info data)
  (lambda (key default)
    (case key
      [(color-lexer) colourizer]
      [(configure-runtime)
       `(#(processing/lang/runtime-config configure #f)) ]
      ;      [(drracket:submit-predicate) read-to-submit?]
      [else default])))
