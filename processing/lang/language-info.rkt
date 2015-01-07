#lang racket

(provide get-language-info)

(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime) 
       `(#(processing/lang/runtime-config configure #f)) ]
;      [(drracket:submit-predicate) read-to-submit?]
      [else default])))
