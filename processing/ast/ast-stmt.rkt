#lang racket/base
(provide (all-defined-out))

(require racket/class
         racket/undefined
         "ast.rkt"
         "../lib/runtime.rkt")

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

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Import of racket modules in processing
(define require% 
  (class stmt%
         (init-field name)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(p-require ,(read (open-input-string name)))))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define global-stmt%
  (class stmt%
         (init-field stmt)

         (inherit ->syntax-object)
         (inherit get-src-info)

         (define/override (->racket)
                          (->syntax-object 
                            (if (or (is-a? stmt empty-stmt%) 
                                    (is-a? stmt block%))
                              `(void ,(node->racket stmt))
                              `(p-active-mode? ,(node->racket stmt)
                                              ',(get-src-info)))))

         (define/override (->type-check) 
                          (node->type-check stmt))

         (super-instantiate ())))

(define global-decl%
  (class stmt%
         (init-field decl)

         (define/override (->racket)
                          (node->racket decl))

         (define/override (->type-check) 
                          (node->type-check decl))

         (super-instantiate ())))

;;; TODO: Check with local-var% they are too similar to exisit
(define global-var%
  (class stmt%
         (init-field modifiers type vars)

         (inherit ->syntax-object set-type-info!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-declaration ,@(node->racket vars))))

         (define/override (->type-check) 
                          (node->type-check vars type))

         (super-instantiate ())))

(define var-decl-id%
  (class stmt%
         (init-field id value)

         (inherit ->syntax-object set-type-info!)

         (define/override (->racket) 
                          (->syntax-object
                            `(,(node->racket id) 
                               ,(node->racket value))))

         (define/override (->type-check type) 
                          (node->type-check value type)
                          (node->type-check id    type)
                          (set-type-info! type))

         (super-instantiate ())))

(define local-var%
  (class stmt%
         (init-field modifiers type vars)

         (inherit ->syntax-object set-type-info!)

         (define/override (->racket)
                          (->syntax-object
                            `(p-declaration ,@(node->racket vars))))

         (define/override (->type-check) 
                          (set-type-info! type)
                          (node->type-check vars type))

         (super-instantiate ())))

(define block%
  (class stmt%
         (init-field stmts)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object
                            `(let ()
                               ,@(node->racket stmts)
                               (void))))

         (define/override (->type-check) 
                          (node->type-check stmts))

         (super-instantiate ())))

(define function-decl% 
  (class stmt%
         (init-field signature body)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(define ,(node->racket signature)
                               (call/ec (lambda (return)
                                          ,(node->racket body))))))

         (define/override (->type-check) 
                          (node->type-check signature)
                          (node->type-check body))

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

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define do-while%
  (class stmt%
         (init-field test body)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(let/ec break
                                     (let loop ()
                                       (let/ec continue ,(node->racket body))
                                       (when ,(node->racket test)
                                         (loop))))))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define while%
  (class stmt%
         (init-field test body)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(let/ec break
                                     (let loop ()
                                       (when ,(node->racket test)
                                         (let/ec continue ,(node->racket body))
                                         (loop))))))

         (define/override (->type-check) #t)

         (super-instantiate ())))


(define for% 
  (class stmt%
         (init-field initialization test increment body)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(let/ec break
                                     ,(node->racket initialization)
                                     (let loop ()
                                       (when ,(node->racket test)
                                         (let/ec continue ,(node->racket body))
                                         ,(node->racket increment)
                                         (loop))))))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define expr-list%
  (class stmt%
         (init-field exprs)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(begin ,@(node->racket exprs))))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define return% 
  (class stmt%
         (init-field expr)

         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            (if(null? expr)
                              `(return (void))
                              `(return ,(node->racket expr)))))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define break% 
  (class stmt%
         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(break (void))))

         (define/override (->type-check) 
                          #t)

         (super-instantiate ())))

(define continue% 
  (class stmt%
         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object 
                            `(continue (void))))

         (define/override (->type-check) 
                          #t)

         (super-instantiate ())))

(define empty-stmt% 
  (class stmt%
         (inherit ->syntax-object)

         (define/override (->racket) (void))

         (define/override (->type-check) #t)

         (super-instantiate ())))

(define undefined%
  (class stmt%
         (inherit ->syntax-object)

         (define/override (->racket)
                          (->syntax-object undefined))

         (define/override (->type-check) #t)

         (super-instantiate ())))

