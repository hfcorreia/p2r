(module runtime racket

  (provide (all-defined-out)
           (all-from-out "operators.rkt"))

  (require racket/undefined
           "operators.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Macro transformations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;
  (define-syntax-rule
    (p-call func ...)
    (func ...))

  ;;; Declaration Operator
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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Print procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )
