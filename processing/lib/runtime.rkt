(module runtime racket

  (provide (all-defined-out)
           (all-from-out "operators.rkt")
           (all-from-out "shapes.rkt")
           (all-from-out "color.rkt")
           (all-from-out "output.rkt"))

  (require racket/undefined
           racket/require
           syntax/readerr
           "operators.rkt"
           "shapes.rkt"
           "color.rkt"
           "output.rkt"
           "../mode.rkt"
           "../name-mangling.rkt"
           (for-syntax "../name-mangling.rkt"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Macro transformations
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ;;; Global Stmt
  ;;;   If in active mode, global stmts are not allowed
  (define-syntax-rule
    (p-global-stmt node src-loc)
    (if (active-mode?)
      (apply raise-read-error (cons "Mixing Static and Active Mode" src-loc))
      (void node)))

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


  )
