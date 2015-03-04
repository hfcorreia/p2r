#lang racket

(provide (all-defined-out))

(require racket/class
         syntax/readerr
         "../mode.rkt"
         "bindings.rkt")

;;; node->racket : (or/c (listof ast-node%) ast-node%)
;;;     -> (or/c (listof ast-node%) ast-node%)
;;; Simplyfies code generation by executing ->racket
;;; over a single or a list of ast-nodes%
(define-syntax-rule
  (node->racket node)
  (if (list? node)
    (map (lambda (elem) (send elem ->racket)) node)
    (send node ->racket)))

(define-syntax-rule
  (node->type-check node)
  (if (list? node)
    (map (lambda (x) (send x ->type-check)) node)
    (send node ->type-check)))

(define-syntax-rule
  (node->bindings node scope)
  (if (list? node)
    (map (lambda (elem) (send elem ->bindings scope)) node)
    (send node ->bindings scope)))

(define-syntax-rule
  (node->print node)
  (if (list? node)
    (map (lambda (elem) (send elem ->print)) node)
    (send node ->print)))
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

         (define/public (->print)
                        `(ast%))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define compilation-unit%
  (class ast-node%
         (init-field ast)

         (inherit ->syntax-object set-scope!)

         (define/public (->repl scope)
                        (set-active-mode! #f)
                        (node->bindings ast scope)
                        (node->type-check ast)
                        (node->racket ast))

         ;; injects call to setup and draw functions if in active-mode
         (define/override (->racket)
                          (if active-mode?
                            (append
                              (node->racket ast)
                              (list
                                (->syntax-object `(p-initialize))))
                            (node->racket ast)))

         (define/override (->type-check)
                          (->syntax-object
                            (node->type-check ast)))

         (define/override (->bindings scope)
                          (add-function scope '() 'void 'println '() '())
                          (set-scope! scope)
                          ;; (pretty-display (->print))
                          (node->bindings ast scope))

         (define/override (->print)
                          `(compilation-unit% ,(node->print ast)))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debug stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define todo-node%
  (class ast-node%
         (init-field msg)

         (inherit ->syntax-object read-error)

         (define/override (->racket)
                          (read-error (format "TODO: ~a" msg)))

         (define/override (->type-check) #f)

         (define/override (->bindings scope) #f)

         (define/override (->print)
                          `(todo% ,msg))

         (super-instantiate ())))
