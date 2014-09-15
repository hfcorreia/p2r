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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TODO: Check operator semantics agains processing's semantics
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Arithmetic Operations
  (define (p-plus arg1 arg2)
    (+ arg1 arg2))

  (define (p-minus arg1 arg2)
    (- arg1 arg2))
  
  (define (p-multiply arg1 arg2)
    (* arg1 arg2))
  
  (define (p-divide arg1 arg2)
    (/ arg1 arg2))

  )
