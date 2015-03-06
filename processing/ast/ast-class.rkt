#lang racket/base

(provide (all-defined-out))

(require racket/class
         "ast.rkt"
         "ast-expr.rkt"
         "../bindings.rkt"
         )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST class nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define class-stmt%
  (class ast-node%
         (inherit read-error)

         (define/override (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

         (define/override (->type-check)
                          (read-error (format "Invalid use of ->type-check ~a"
                                              this)))

         (define/override (->bindings scope)
                          (read-error (format "Invalid use of ->bindings ~a" this)))

         (super-instantiate ())))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define class-node%
  (class class-stmt%
         (init-field name body)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-class ,(node->racket name)
                                      ,@(node->racket body))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings body scope))

         (define/override (->print)
                          `(class-node%))
         (super-instantiate ())))

(define new-node%
  (class class-stmt%
         (init-field name args)

         (inherit ->syntax-object get-src-info set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-new-class
                               ,(node->racket name)
                               ,@(node->racket args))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings name scope)
                          (node->bindings args scope))

         (define/override (->print)
                          `(new-node%))
         (super-instantiate ())))

(define class-field%
  (class class-stmt%
         (init-field modifiers type vars)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-class-field
                               ,@(map (lambda (var)
                                        (list
                                          (node->racket (car var))
                                          (node->racket (cadr var))))
                                      vars))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (map (lambda (var)
                                 (node->bindings (car var) scope)
                                 (node->bindings (cadr var) scope))
                               vars))

         (define/override (->print)
                          `(class-field%))
         (super-instantiate ())))

(define method-decl%
  (class class-stmt%
         (init-field modifiers return-type id parameters throws body)

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(define/public (,(node->racket id)
                                              ,@(node->racket parameters))
                                            (call/ec (lambda (return)
                                                       ,(node->racket body))))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope) #t)

         (define/override (->print)
                          `(method-decl%))
         (super-instantiate ())))

(define formal-parameter%
  (class class-stmt%
         (init-field final type id)

         (inherit ->syntax-object set-scope!)

         (define/public (get-type) type)

         (define/override (->racket)
                          (->syntax-object (node->racket id)))

         (define/override (->type-check)
                          (send id set-type! type))

         (define/override (->bindings scope)
                          (add-variable scope `(,final) type
                                                (send id get-id))
                          (set-scope! scope)
                          (node->bindings id scope))

         (define/override (->print)
                          `(formal-parameter% ,final
                                              ,(send type get-type)
                                              ,(node->print id)))
         (super-instantiate ())))

(define this-node%
  (class class-stmt%

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            this))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope))

         (define/override (->print)
                          `(this-node%))
         (super-instantiate ())))

(define field-acces%
  (class class-stmt%
         (init-field primary id)

         (define/public (get-id)      (node->racket id))
         (define/public (get-primary) (node->racket primary))

         (inherit ->syntax-object set-scope!)

         (define/override (->racket)
                          (->syntax-object
                            `(get-field ,(node->racket id)
                                        ,(node->racket primary))))

         (define/override (->type-check) #t)

         (define/override (->bindings scope)
                          (set-scope! scope)
                          (node->bindings primary scope)
                          (node->bindings id scope))

         (define/override (->print)
                          `(field-acces%))
         (super-instantiate ())))
