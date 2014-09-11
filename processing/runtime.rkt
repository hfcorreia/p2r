(module runtime racket

  (provide (all-defined-out))
  
  (define-syntax-rule
    (dispatch func ...)
    (func ...))
  
  ;; Poor man's implementation
  (define (println arg)
    (if (not (null? arg))
    (displayln arg)
    (displayln "")))
  
  
  )
