(module runtime racket

  (provide (all-defined-out)
           (all-from-out "operators.rkt")
           (all-from-out "processing.rkt"))

  (require racket/undefined
           racket/require
           "operators.rkt"
           "processing.rkt"
           "name-mangling.rkt"
           (for-syntax "name-mangling.rkt"))

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
  ;;; require racket modules
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax-rule
    (p-require require-spec)
    (require (filtered-in racket->java require-spec)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Print procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (print . args)
    (display (build-seperated-string (reverse (map arg->string args)) #\space)))

  (define (println . args)
    (displayln (build-seperated-string (reverse (map arg->string args)) #\space)))

  ;;; Converts print arguments to be correctly printed
  (define (arg->string arg)
    (format "~a"
            (cond
              [(and (boolean? arg) arg) "true"]
              [(and (boolean? arg) (not arg)) "false"]
              [(void? arg) ""]
              [(not (null? arg)) arg]
              [else  ""])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Aux procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Given a list of strings generates a string seprated by char
  (define (build-seperated-string lst char)
    (cond 
      [(null? lst) ""]
      [(eq? (length lst) 1) (car lst)]
      [else (format "~a~a~a" (car lst) char (build-seperated-string (cdr lst) char))]))

  )
