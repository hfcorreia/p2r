#lang racket
(provide (all-defined-out))

(require racket/class
         racket/undefined

         "ast.rkt"
         "ast-expr.rkt"
         "errors.rkt"
         "bindings.rkt"
         "types.rkt"
         "../mode.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST stmt nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ast-node abstraction for statments
(define stmt%
  (class ast-node%
         (inherit read-error)

         (define/override (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

         (define/override (->type-check)
                          (read-error (format "Invalid use of ->type-check ~a" this)))

         (define/override (->bindings scope)
                          (read-error (format "Invalid use of ->bindings ~a" this)))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import of racket modules in processing
(define require%
  (class stmt%
         (init-field name)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-require ,(read (open-input-string name)))))

         ;; Everything from racket is an Object?
         (define/override (->type-check) #t)

         ;; Possibly introduces bindings
         (define/override (->bindings scope)
                          (set-scope! scope))

         (super-instantiate ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define global-stmt%
  (class stmt%
         (init-field stmt)

         (inherit ->syntax-object set-scope! get-src-info)

         (define/override (->racket)
                          (->syntax-object
                            (if (or (is-a? stmt empty-stmt%)
                                    (is-a? stmt block%))
                              `(void ,(node->racket stmt))
                              `(p-active-mode? ,(node->racket stmt)
                                               ,active-mode?
                                               ',(get-src-info)))))

         (define/override (->type-check)
                          (node->type-check stmt))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings stmt scope))

         (super-instantiate ())))

(define global-decl%
  (class stmt%
         (init-field decl)

         (inherit set-scope!)

         (define/override (->racket)
                          (node->racket decl))

         (define/override (->type-check)
                          (node->type-check decl))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings decl scope))

         (super-instantiate ())))

(define var-decl%
  (class stmt%
         (init-field modifiers type vars)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-declaration
                               ,@(map (lambda (var)
                                        (list (node->racket (car var))
                                              (node->racket (cadr var))))
                                      vars))))

         (define/override (->type-check)
                          (map (lambda (var)
                                 (send (car var) set-type! type) ; set types
                                 (node->type-check (cadr var))
                                 (check-node-type (cadr var)))
                               vars))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (map (lambda (var)
                                 (add-variable scope modifiers type (send (car var) get-id))
                                 (node->bindings (car var) scope)
                                 (node->bindings (cadr var) scope))
                               vars))

         ;;; check-node-type: ast-node% -> (or/c any error)
         ;;; dispatch according to varible type
         (define (check-node-type node)
           (cond
             [(is-a? node literal%) (check-literal node)]
             [(is-a? node binary-op%) (check-literal node)]
             [(is-a? node name%) (check-literal node)]
             ; array
             ; reference-type
             [else #t]))

         ;; check-literal: type literal% -> (or/c void error)
         ;; checks if types are the same or can be promoted
         ;; produces type-error otherwise
         (define (check-literal literal)
           (let ([literal-type (send literal get-type)])
             (cond
            ;; [(send literal-type undef-type?)
            ;;  (send literal set-type! literal-type)]
               [(or (type=? type literal-type)
                    (widening-primitive-conversion? type literal-type))
                (send literal set-type! type)]
               [else (type-conversion-error literal literal-type type)])))

         (super-instantiate ())))

(define block%
  (class stmt%
         (init-field stmts)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(let ()
                               ,@(node->racket stmts)
                               (void))))

         (define/override (->type-check)
                          (node->type-check stmts))

         (define/override (->bindings scope)
                          (let ([local-scope (make-object local-scope% scope)])
                            (set-scope! local-scope)
                            (node->bindings stmts local-scope)))

         (super-instantiate ())))

(define function-decl%
  (class stmt%
         (init-field modifiers return-type id parameters throws body)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(define (,(node->racket id)
                                       ,@(node->racket parameters))
                               (call/ec (lambda (return)
                                          ,(node->racket body))))))

         (define/override (->type-check)
                          (node->type-check body))

         (define/override (->bindings scope)
                          (let ([local-scope      (make-object local-scope% scope)]
                                [parameter-types  (map (lambda (x)
                                                         (send x get-type))
                                                       parameters)])
                            (set-scope! local-scope)
                            (add-function scope modifiers return-type
                                          (send id get-id)
                                          parameter-types throws)
                            (node->bindings parameters local-scope)
                            (node->bindings body local-scope)))

         (super-instantiate ())))

(define if%
  (class stmt%
         (init-field test then else)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object
                            `(if ,(node->racket test)
                               ,(node->racket then)
                               ,(if (null? else)
                                  (void)
                                  (node->racket else)))))

         (define/override (->type-check)
                          (node->type-check test)
                          (node->type-check then)
                          (and (not (null? else))
                               (node->type-check else)))

         (define/override (->bindings scope)
                          (node->bindings then scope)
                          (and (not (null? else))
                               (node->bindings else scope)))

         (super-instantiate ())))

(define do-while%
  (class stmt%
         (init-field test body)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(let/ec break
                                     (let loop ()
                                       (let/ec continue ,(node->racket body))
                                       (when ,(node->racket test)
                                         (loop))))))

         (define/override (->type-check)
                          (node->type-check test)
                          (node->type-check body))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings body scope))

         (super-instantiate ())))

(define while%
  (class stmt%
         (init-field test body)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(let/ec break
                                     (let loop ()
                                       (when ,(node->racket test)
                                         (let/ec continue ,(node->racket body))
                                         (loop))))))

         (define/override (->type-check)
                          (node->type-check test)
                          (node->type-check body))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings body scope))

         (super-instantiate ())))


(define for%
  (class stmt%
         (init-field initialization test increment body)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(let/ec break
                                     ,(node->racket initialization)
                                     (let loop ()
                                       (when ,(node->racket test)
                                         (let/ec continue ,(node->racket body))
                                         ,(node->racket increment)
                                         (loop))))))

         (define/override (->type-check)
                          (node->type-check initialization)
                          (node->type-check test)
                          (node->type-check increment)
                          (node->type-check body))

         (define/override (->bindings scope)
                          (let ([local-scope (make-object local-scope% scope)])
                            (set-scope! local-scope)
                            (node->bindings initialization local-scope)
                            (node->bindings body local-scope)))

         (super-instantiate ())))

(define expr-list%
  (class stmt%
         (init-field exprs)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object
                            `(begin ,@(node->racket exprs))))

         (define/override (->type-check)
                          (node->type-check exprs))

         (super-instantiate ())))

(define return%
  (class stmt%
         (init-field expr)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            (if(null? expr)
                              `(return (void))
                              `(return ,(node->racket expr)))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (super-instantiate ())))

(define break%
  (class stmt%
         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(break (void))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (super-instantiate ())))

(define continue%
  (class stmt%
         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(continue (void))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (super-instantiate ())))

(define empty-stmt%
  (class stmt%
         (inherit ->syntax-object set-scope!)

         (define/override (->racket) (void))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (super-instantiate ())))

