#lang racket
(provide (all-defined-out))

(require racket/class
         racket/undefined

         "ast.rkt"
         "ast-expr.rkt"
         "errors.rkt"
         "types.rkt"
         "../bindings.rkt"
         "../scopes.rkt"
         "../mode.rkt"
         "../util.rkt")

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

         (inherit ->syntax-object set-scope! get-src-info)

         (define bindings (list))

         (define/override (->racket)
                          (->syntax-object
                            `(p-require ,(read (open-input-string name))
                                        ,(mangle-bindings bindings))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (add-exported-bindings scope (read (open-input-string name)))
                          (set-scope! scope))

         (define (mangle-bindings bindings)
           (map (lambda (x)
                  (let ([mangled (racket->java (symbol->string (car x)))])
                    (list (symbol->string (car x))
                          (if (cdr x)
                            (symbol->string (mangle-function-id
                                              (string->symbol mangled)
                                              (build-list (cdr x)
                                                          (lambda (y)
                                                            'Object))))
                            mangled))))
                bindings))

         ;; types: int -> list/of type%
         (define (types n)
           (build-list n (lambda (x) (create-type 'Object))))

         (define (add-exported-bindings scope mod)

           ;; arity: symbol -> or/c int #f
           ;; given the exported binding symbol determines the arity
           (define (arity sym)
             (let ([p (dynamic-require mod sym)])
               (and (procedure? p) (procedure-arity p))))


           ;; add-exported-bindings: sym
           ;; given and exported symbol generates the respective binding%
           ;; and adds it to the current scope
           (define (add-exported-binding sym)
             (let ([id (make-object identifier% null
                                    (racket->java (symbol->string sym))
                                    (get-src-info))]
                   [arity (arity sym)]
                   [type (create-type 'Object)])
               (set! bindings
                 (append (list (cons sym arity)) bindings))
               (if (not arity)
                 (add-binding scope ('() type : id))
                 (add-binding scope ('() id (types arity)) -> (type '())))))

           (dynamic-require mod #f)
           (let-values ([(vars syntax) (module->exports mod)])
             (when (not (null? vars))
               (map add-exported-binding (map car (cdar vars))))))

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
                                 (add-binding scope (modifiers type : (car var)))
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
                            `(p-block ,@(node->racket stmts))))

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
         (init-field mods ret-type id args throws body)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(define (,(build-mangled-id)
                                       ,@(node->racket args))
                               (let/ec return
                                      ,(node->racket body)))))

         (define/override (->type-check)
                          (node->type-check args)
                          (node->type-check body))

         (define/override (->bindings scope)
                          (let ([local-scope (make-object local-scope% scope ret-type)]
                                [args-types  (map (lambda (x) (send x get-type)) args)])
                            (set-scope! scope)
                            (add-binding scope
                                         (mods id args-types)
                                         ->
                                         (ret-type throws))
                            (node->bindings args local-scope)
                            (node->bindings body local-scope)))

         (define (build-mangled-id)
           (send id
                 mangled-id
                 (mangle-function-id
                   (send id get-id)
                   (map (lambda (x) (send (send x get-type) get-type)) args))))


         (define/override (->print)
                          `(function% ,mods ,(send ret-type get-type)
                                      ,(node->print id)
                                      ,(node->print args) ,throws
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
                          (let ([type (send test get-type)])
                            (unless (send type boolean-type?)
                              (boolean-conversion-error test type)))
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
                          (let ([type (send test get-type)])
                            (unless (send type boolean-type?)
                              (boolean-conversion-error test type)))
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
                          (let ([type (send test get-type)])
                            (unless (send type boolean-type?)
                              (boolean-conversion-error test type)))
                          (node->type-check body))

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings test scope)
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
                          (let ([type (send test get-type)])
                            (unless (send type boolean-type?)
                              (boolean-conversion-error test type)))
                          (node->type-check increment)
                          (node->type-check body))

         (define/override (->bindings scope)
                          (let ([local-scope (make-object local-scope%
                                                          scope
                                                          (send scope return-type))])
                            (set-scope! local-scope)
                            (node->bindings initialization local-scope)
                            (node->bindings test local-scope)
                            (node->bindings increment local-scope)
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

