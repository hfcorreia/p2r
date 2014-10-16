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
  ;;; import racket modules
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-syntax p-import
    (syntax-rules ()
      [(_ id) id]
      [(_ 'Racket full-name)
       (string->symbol 
         (build-seperated-string (cdr full-name) #\/))]
      [(_ 'PLaneT full-name version)
       (string->symbol 
         (if (eq? (length (cdr full-name)) 2)
           ; foo/bar -> foo/bar:2:0
           (string-append 
             (build-seperated-string (cdr full-name) #\/)
             version)
           ; foo/bar/bazz -> foo/bar:2:0/bazz
           (let ([vec (list->vector (cdr full-name))])
             (vector-set! vec 1 (string-append 
                                  (symbol->string (vector-ref vec 1))
                                  version))
             (build-seperated-string (vector->list vec) #\/))))]))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Print procedures
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (p-print . args)
    (display (build-seperated-string (reverse (map arg->string args)) #\space)))

  (define (p-println . args)
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  )
