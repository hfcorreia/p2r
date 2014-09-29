(module runtime racket

  (provide (all-defined-out))

  (require racket/undefined)

  (define-syntax-rule
    (p-call func ...)
    (func ...))

  (define (p-print . args)
    (display (string-with-spaces (reverse (map arg->string args)))))

  (define (p-println . args)
    (displayln (string-with-spaces (reverse (map arg->string args)))))

  ;;; Aux
  (define (string-with-spaces lst)
    (cond 
      [(null? lst) ""]
      [(eq? (length lst) 1) (car lst)]
      [else (format "~a ~a" (car lst) (string-with-spaces (cdr lst)))]))

  ;;; Aux
  (define (arg->string arg)
    (format "~a"
            (cond
              [(and (boolean? arg) arg) "true"]
              [(and (boolean? arg) (not arg)) "false"]
              [(void? arg) ""]
              [(not (null? arg)) arg]
              [else  ""])))

  ;; Declaration Operator
  (define-syntax p-declaration
    (syntax-rules ()
      [(_ id)
       (define id undefined)]
      [(_ id expr)
       (define id expr)]))


  ;;; Assigments
  (define-syntax p-assignment 
    (syntax-rules ()
      [(_ id expr)
       (begin
         (set! id expr)
         id)]
      [(_ op id expr)
       (begin
         (set! id (op id expr))
         id)]))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; TODO: Check operator semantics against processing's semantics
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

  ;;; TODO: emulate java behavior
  (define (p-shiftl arg1 arg2)
    (arithmetic-shift arg1 arg2))

  ;;; TODO: emulate java behavior
  (define (p-shiftr arg1 arg2)
    (arithmetic-shift arg1 (- arg2)))

  ;;; TODO: emulate java behavior
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

  (define (p-lt arg1 arg2)
    (< arg1 arg2))

  (define (p-gt arg1 arg2)
    (> arg1 arg2))

  (define (p-lt-eq arg1 arg2)
    (<= arg1 arg2))

  (define (p-gt-eq arg1 arg2)
    (>= arg1 arg2))

  (define (p-eq arg1 arg2)
    (eq? arg1 arg2))

  (define (p-not-eq arg1 arg2)
    (not (eq? arg1 arg2)))

  (define (p-instanceof arg1 arg2)
    (format "TODO: ~a instanceof ~a" arg1 arg2))
  )
