#lang racket

(provide (all-defined-out))

(require racket/undefined
         racket/require
         syntax/readerr

         "name-mangling.rkt"
         (for-syntax "name-mangling.rkt")

         "processing/api.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Checks if setup and draw functions are bound and calls them if so
(define-syntax (p-initialize stx)
  (syntax-case stx ()
    [(_)
     (with-syntax
       ([setup (datum->syntax stx 'setup-fn)]
        [draw  (datum->syntax stx 'draw-fn)])
       (cond
         [(and (identifier-binding #'setup 0)  (identifier-binding #'draw 0))
          #'(begin (setup) (draw))]
         [(identifier-binding #'setup 0) #'(setup)]
         [(identifier-binding #'draw 0)  #'(draw)]
         [else #'(void)]))]))

;;; Call a global method
(define-syntax p-call
  (syntax-rules ()
    [(_ #:call method-name)
     (method-name)]
    [(_ #:call method-name args ...)
     (method-name args ...)]
    [(_ #:send full-name method-name)
     (send full-name method-name)]
    [(_ #:send full-name method-name args ...)
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
    [(_ op left expr) (left op expr)]))

;;; Left value
(define-syntax p-left-value
  (syntax-rules ()
    [(_ arg #:name)
     (lambda (op expr) (set! arg (op arg expr)) arg)]
    [(_ arg obj #:qual-name)
     (lambda (op expr) (set-field! arg obj (op arg expr)) (get-field arg obj))]
    [(_ arg obj #:field)
     (lambda (op expr) (set-field! arg obj (op arg expr)) (get-field arg obj))]
    [(_ arg pos #:array)
     (lambda (op expr) (vector-set! arg pos (op arg expr)) (vector-ref arg pos))]))

;;; Global Stmt
;;;   If in active mode, global stmts are not allowed
(define-syntax-rule
  (p-active-mode? node active-mode? src-loc)
  (if (active-mode?)
    (apply raise-read-error (cons "Mixing Static and Active Mode" src-loc))
    (void node)))

;;; Arrays
(define-syntax-rule
  (p-vector (dim ...) init-val)
  (make-n-vector (list dim ...) init-val))

;;; Build an identifier
(define-syntax-rule
  (p-build-identifier func id)
  (func id))

;;; check if a identifier is a vector, if true get the vector's length else
;;; get field length of the identifier
(define-syntax-rule
  (p-array-length id len)
  (if (vector? id)
    (vector-length id)
    (get-field len id)))


;;; Builds a n-dimentional vector give a list of values and a initial value
(define (make-n-vector lst val)
  (define (aux lst)
    (if (null? lst)
      val
      (make-vector (car lst) (aux (cdr lst)))))
  (aux lst))
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

(define-syntax (p-new-class stx)
  (syntax-case stx ()
    [(_ id args ...)
     #'(make-object id args ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; require racket modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule
  (p-require require-spec)
  (require (filtered-in racket->java require-spec)))
