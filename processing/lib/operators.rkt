(module processing-api/operators racket

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Processing Operators
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (p-assign left right)
    right)

  (define (p-add left right)
    (if (or (string? left)
            (string? right))
      (format "~a~a" left right)
      (+ left right)))

  (define (p-sub left right)
    (- left right))

  (define (p-mul left right)
    (* left right))

  (define (p-div left right)
    (/ left right))

  (define (p-mod left right)
    (modulo left right))

  (define (p-bit-and left right)
    (bitwise-and left right))

  (define (p-bit-xor left right)
    (bitwise-xor left right))

  (define (p-bit-or left right)
    (bitwise-ior left right))

  (define (p-and left right)
    (and left right))

  (define (p-or left right)
    (or left right))

  (define (p-shiftl left right)
    (arithmetic-shift left right))

  (define (p-shiftr left right)
    (arithmetic-shift left (- right)))

  (define (p-shiftr-zero left right)
    (+ (arithmetic-shift left (- right))
       (arithmetic-shift 2 (bitwise-not right))))

  (define (p-pos arg)
    arg)

  (define (p-neg arg)
    (- arg))

  (define (p-bit-not arg)
    (bitwise-not arg))

  (define (p-not arg)
    (not arg))

  (define (p-lt left right)
    (< left right))

  (define (p-gt left right)
    (> left right))

  (define (p-lt-eq left right)
    (<= left right))

  (define (p-gt-eq left right)
    (>= left right))

  (define (p-eq left right)
    (equal? left right))

  (define (p-not-eq left right)
    (not (eq? left right)))

  (define (p-instanceof left right)
    (error "instanceof not implemented!"))

  (define-syntax-rule
    (p-pre-inc arg)
    (begin
      (set! arg (+ arg 1))
      arg))

  (define-syntax-rule
    (p-pre-dec arg)
    (begin
      (set! arg (- arg 1))
      arg))

  (define-syntax-rule
    (p-pos-inc arg)
    (begin
      (set! arg (+ arg 1))
      (- arg 1)))

  (define-syntax-rule
    (p-pos-dec arg)
    (begin
      (set! arg (- arg 1))
      (+ arg 1)))
  )
