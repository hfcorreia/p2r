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

  ;;; embeded applet behavior
  ;define-syntax (p-applet stx)
  ;(syntax-case stx ()
  ;  [(_) '()]
  ;  [(_ code ...) 
  ;   (with-syntax
  ;     ([(applet code ...) 
  ;       #'(class object%
  ;                (define/public (draw) code ... (void))
  ;                (super-instantiate ()))])
  ;     #'(send (make-object (applet code ...)) draw))]))

  ;;; Call a global method
  (define-syntax p-call
    (syntax-rules ()
      [(_ method-name) 
       (method-name)]
      [(_ method-name args ...) 
       (method-name args ...)]))

  ;;; Call a method from an object
  (define-syntax p-send
    (syntax-rules ()
      [(_ full-name method-name) 
       (send full-name method-name)]
      [(_ full-name method-name args ...) 
       (send full-name method-name args ...)]))

  ;;; Declaration Operator
  (define-syntax (p-declaration stx)
    (syntax-case stx ()
      [(_ elem ...)
       (with-syntax 
         ([(ids ...)
           (datum->syntax 
             stx
             (map (lambda (x)
                    (car (syntax-e x)))
                  (syntax->list #'(elem ...))))]
          [(vals ...) 
           (datum->syntax 
             stx
             (map (lambda (x) (cadr (syntax-e x))) 
                  (syntax->list #'(elem ...))))])
         #'(define-values (ids ...) (values vals ...)))]))

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
  ;;; Class macros
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax p-class
    (syntax-rules ()
      [(_ id body ...) 
       (define id 
         (class object% 
                body ...
                (super-instantiate())))]))

  (define-syntax-rule 
    (p-class-field [id val] ...)
    (field [id val] ...))
    
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
