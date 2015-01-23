#lang racket/base

(provide (all-defined-out))

(require racket/class
         syntax/readerr
         "../lib/runtime.rkt")


;;; node->racket : (or/c (listof ast-node%) ast-node%) 
;;;     -> (or/c (listof ast-node%) ast-node%)
;;; Simplyfies code generation by executing ->racket 
;;; over a single or a list of ast-nodes%
(define-syntax-rule
  (node->racket node)
  (if (list? node)
    (map (lambda (elem) (send elem ->racket)) node)
    (send node ->racket)))

(define-syntax node->type-check
  (syntax-rules ()
    [(_ node)
     (if (list? node)
         (map (lambda (x) (send x ->type-check)) node)
         (send node ->type-check))]
    [(_ node type)
     (if (list? node)
         (map (lambda (x) (send x ->type-check type)) node)
         (send node ->type-check type))]))

(define-syntax-rule
  (node->bindings node scope)
  (if (list? node)
    (map (lambda (elem) (send elem ->bindings scope)) node)
    (send node ->bindings scope)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST struct definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Root of the AST. All nodes are a subclass of ast-node%.
;;; Encapsulates source info and all required functions to generate
;;; syntax-objects.
(define ast-node%
  (class object%
         (init-field [src-info null])
         (field [scope null])

         (define/public (get-src-info) src-info)
         (define/public (get-scope) scope)
         (define/public (set-scope! new-scope) (set! scope new-scope))

         ;; read-err: string? -> exn:fail:read
         ;; raises an exception with source of the expression
         (define/public (read-error msg)
                        (apply raise-read-error (cons msg src-info)))


         ;; ->syntax-object : datum? -> syntax-object?
         ;; converts the datum to a syntax object using stored src-info
         (define/public (->syntax-object datum)
                        (datum->syntax #'here
                                       datum
                                       (and (not (null? src-info)) src-info)
                                       (read-syntax #f 
                                                    (open-input-string "orig")) 
                                       ))

         ;; ->racket: -> syntax-object?
         ;; generates the syntax-object relative to the node
         (define/public (->racket)
                        (read-error (format "Invalid use of ->racket ~a" this)))

         ;; ->type-check: -> (or/c #t read-error?)
         ;; type checks the generated ast
         (define/public (->type-check)
                        (read-error (format "Invalid use of ->type-check ~a"
                                            this)))

         ;; ->bindings: (is-a? binding-scope<%>) -> 
         (define/public (->bindings scope) 
                        (read-error (format "Invalid use of ->bindings ~a" this)))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define todo? #f)
(define (clear-todo)
  (set! todo? #f))

(define todo-node%
  (class ast-node%
         (init-field child msg)

         (inherit ->syntax-object read-error)

         (define/override (->racket)
                          (clear-todo)
                          (->syntax-object
                            (aux child)))

         (define/override (->type-check [type null]) #t)

         ;; aux funtion
         (define (aux child)
           (cond 
             [(list? child) (map (lambda (node)
                                   (aux node))
                                 child)]
             [(is-a? child ast-node%) (send child ->racket)]
             [else child]))

         (super-instantiate ())))
