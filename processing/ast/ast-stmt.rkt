#lang racket
(provide (all-defined-out))

(require racket/class
         racket/undefined

         "ast.rkt"
         "ast-expr.rkt"
         "errors.rkt"
         "../bindings.rkt"
         "types.rkt"
         "../mode.rkt"
         "../name-mangling.rkt")

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

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (add-exported-bindings scope (read (open-input-string name)))
                          (set-scope! scope))


         (define (add-exported-bindings scope mod)
           ;; get the aritiy of a given binding
           (define (arity binding)
             (let ([p (dynamic-require mod binding)])
               (if (procedure? p) (procedure-arity p) 0)))

           ;; generate list of Object types
           (define (object-list n)
             (build-list n (lambda (x) (create-type 'Object))))

           (define (add-binding binding)
             (let ([id (string->symbol (racket->java (symbol->string binding)))]
                   [arity (arity binding)]
                   [type (create-type 'Object)])
               (if (equal? arity 0)
                 (add-variable scope '() type id)
                 (add-function scope '() type id (object-list arity) '()))))

           (dynamic-require mod #f)
           (let-values ([(vars syntax) (module->exports mod)])
             (map add-binding (map car (cdar vars)))))

         (define/override (->print)
                          `(require% ,(node->print name)))

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

         (define/override (->print)
                          `(global-stmt% ,(node->print stmt)))

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

         (define/override (->print)
                          `(global-decl% ,(node->print decl)))

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


         ;; check-node-type: ast-node% -> (or/c any error)
         ;; checks if types are the same or can be promoted
         ;; produces type-error otherwise
         (define (check-node-type node)
           (let ([node-type (send node get-type)])
             (cond
               [(or (type=? type node-type)
                    (object-type? type node-type)
                    (widening-primitive-conversion? type node-type))
                (send node set-type! type)]
               [else (type-conversion-error node node-type type)])))

         (define/override (->print)
                          `(var-decl% ,modifiers ,type
                                      ,@(map (lambda (x)
                                               (list
                                                 (node->print (car x))
                                                 (node->print (cadr x))))
                                             vars)))
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
                          (let ([local-scope (make-object local-scope%
                                                          scope
                                                          (send scope return-type))])
                            (set-scope! local-scope)
                            (node->bindings stmts local-scope)))

         (define/override (->print)
                          `(block% ,@(node->print stmts)))

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
                          (node->type-check parameters)
                          (node->type-check body))

         (define/override (->bindings scope)
                          (let ([local-scope (make-object local-scope% scope return-type)]
                                [parameter-types  (map (lambda (x)
                                                         (send x get-type))
                                                       parameters)])
                            (set-scope! scope)
                            (add-function scope
                                          modifiers
                                          return-type
                                          (send id get-id)
                                          parameter-types
                                          throws)
                            (send id mangle-id! parameter-types)
                            (node->bindings parameters local-scope)
                            (node->bindings body local-scope)))


         (define/override (->print)
                          `(function% ,modifiers ,(send return-type get-type)
                                      ,(node->print id)
                                      ,(node->print parameters) ,throws
                                      ,(node->print body)))


         (super-instantiate ())))

(define if%
  (class stmt%
         (init-field test then else)

         (inherit ->syntax-object set-scope!)

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
                          (set-scope! scope)
                          (node->bindings test scope)
                          (node->bindings then scope)
                          (and (not (null? else))
                               (node->bindings else scope)))

         (define/override (->print)
                          `(if% ,(node->print test)
                                ,(node->print then)
                                ,(and (not (null? else))
                                      (node->print else))))

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

         (define/override (->print)
                          `(do-while%))

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

         (define/override (->print)
                          `(while%))

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
                          (let ([local-scope (make-object local-scope%
                                                          scope
                                                          (send scope return-type))])
                            (set-scope! local-scope)
                            (node->bindings initialization local-scope)
                            (node->bindings body local-scope)))

         (define/override (->print)
                          `(for%))

         (super-instantiate ())))

(define expr-list%
  (class stmt%
         (init-field exprs)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(begin ,@(node->racket exprs))))

         (define/override (->type-check)
                          (node->type-check exprs))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings exprs scope))

         (define/override (->print)
                          `(expr-list%))

         (super-instantiate ())))

(define return%
  (class stmt%
         (init-field expr)

         (inherit ->syntax-object set-scope! get-scope)

         (define/override (->racket)
                          (->syntax-object
                            (if(null? expr)
                              `(return (void))
                              `(return ,(node->racket expr)))))

         (define/override (->type-check)
                          (let ([return-type (get-return-type)])
                            (node->type-check expr)
                            (if (or (type=? (send expr get-type)
                                            return-type)
                                    (object-type? (send expr get-type)
                                                  return-type)
                                    (widening-primitive-conversion?
                                      return-type
                                      (send expr get-type)))
                              (send expr set-type! return-type)
                              (type-conversion-error expr
                                                     (send expr get-type)
                                                     return-type))))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings expr scope))

         (define (get-return-type)
           (let ([return-type (send (get-scope) return-type)])
             (if (not (false? return-type))
               return-type
               (return-error this))))

         (define/override (->print)
                          `(return% ,(node->print expr)))

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

         (define/override (->print)
                          `(break%))

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

         (define/override (->print)
                          `(continue%))

         (super-instantiate ())))

(define empty-stmt%
  (class stmt%
         (inherit ->syntax-object set-scope!)

         (define/override (->racket) (void))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (define/override (->print)
                          `(empty-stmt%))

         (super-instantiate ())))

