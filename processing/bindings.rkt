#lang racket

(provide (all-defined-out))

(require "ast/errors.rkt"
         "ast/types.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax add-binding
  (syntax-rules ()
    ;; scope% (list/of symbol) symbol (list/of type%) -> type% exn%
    [(_ scope (mods id types) -> (ret-type throws))
     (send scope
           add-binding
           (make-object function-binding% mods ret-type types throws id))]
    ;; scope% (list/of symbol) type% symbol
    [(_ scope (mods type : id))
     (send scope
           add-binding
           (make-object variable-binding% mods type id))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binding object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define binding%
  (class object%
         (init-field id)

         (define/public (get-id) id)

         (super-instantiate ())))

(define function-binding%
  (class binding%
         (init-field modifiers return-type args throws)
         (inherit get-id)

         (define/public (get-modifiers) modifiers)
         (define/public (get-return-type) return-type)
         (define/public (get-arity) (length args))
         (define/public (get-args) args)
         (define/public (get-mangled-id)
                        (send (get-id)
                              mangled-id
                              (mangle-function-id
                                (send (get-id) get-id)
                                (map (lambda (x) (send x get-type)) args))))

         (super-instantiate ())))

(define variable-binding%
  (class binding%
         (init-field modifiers type)

         (define/public (get-modifiers) modifiers)
         (define/public (get-type) type)

         (super-instantiate ())))

(define (variable-binding? binding)
  (or (is-a? binding variable-binding%)))

(define (function-binding? binding)
  (is-a? binding function-binding%))

;; variable? : list/of binding -> boolean
;; check if a variable binding exists in the binding list
(define (exists-variable? lst)
  (andmap variable-binding? lst))

;; check-binds : binding% list/of binding%
;; checks if the binding can be added to the bindings list
;; returning the appended list
(define (check-binds bind lst)
  (let ([id (send bind get-id)])
    (cond
      [(and (function-binding? bind)
            (duplicate-function? bind lst))
       (duplicate-method id
                         (send id get-id)
                         (map (lambda (x)
                                (send x get-type))
                              (send bind get-args)))]
      [(and (variable-binding? bind)
            (exists-variable? lst))
       (duplicate-variable id (send id get-id))]
      [else (append `(,bind) lst)])))

(define (duplicate-function? binding binding-list)
  (ormap (lambda (x)
           (and (not (variable-binding? x))
                (equal? (send binding get-arity) (send x get-arity))
                (andmap type=? (send binding get-args) (send x get-args))))
         binding-list))


;; applicable-function: list/of type% list/of bindings
;; recevies a function's arg types and a list of possible applicable function
(define (applicable-function args bindings)
  (filter (lambda (binding)
            (signature-equals? args (send binding get-args)))
          bindings))

;; promotable-function: list/of type% list/of bindings
;; recevies a function's arg types and a list of possible promotable function
(define (promotable-function args bindings)
  (filter (lambda (binding)
            (signature-promotable? args (send binding get-args)))
          bindings))
