(module runtime racket

  (provide (all-defined-out))

  (define-syntax-rule
    (dispatch func ...)
    (func ...))

  ;; Poor man's implementation
  (define (println arg)
    (displayln 
      (cond
        [(and (boolean? arg) arg) "true"]
        [(and (boolean? arg) (not arg)) "false"]
        [(not (null? arg)) arg]
        [else  ""])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TODO: Check operator semantics agains processing's semantics
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Arithmetic Operations
  (define (p-add arg1 arg2)
    (+ arg1 arg2))

  (define (p-sub arg1 arg2)
    (- arg1 arg2))

  (define (p-mul arg1 arg2)
    (* arg1 arg2))

  (define (p-div arg1 arg2)
    (/ arg1 arg2))

  (define (p-mod arg1 arg2)
    (modulo arg1 arg2))

  (define (p-bit-and arg1 arg2)
    (bitwise-and arg1 arg2))

  (define (p-bit-xor arg1 arg2)
    (bitwise-xor arg1 arg2))

  (define (p-bit-or arg1 arg2)
    (bitwise-ior arg1 arg2))

  (define (p-and arg1 arg2)
    (and arg1 arg2))

  (define (p-or arg1 arg2)
    (or arg1 arg2))

  ;; TODO: emulate java behavior
  (define (p-shiftl arg1 arg2)
    (arithmetic-shift arg1 arg2))

  ;; TODO: emulate java behavior
  (define (p-shiftr arg1 arg2)
    (arithmetic-shift arg1 (- arg2)))

  ;; TODO: emulate java behavior
  (define (p-shiftr-zero arg1 arg2)
    (arithmetic-shift arg1 arg2))

  (define (p-pos arg)
    arg)

  (define (p-neg arg)
    (- arg))

  (define (p-bit-not arg)
    (bitwise-not arg))

  (define (p-not arg)
    (not arg))

  )
