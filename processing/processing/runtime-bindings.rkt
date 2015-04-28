#lang racket

(provide (all-defined-out))

(require "../util.rkt"
         (for-syntax "../util.rkt")
         racket/runtime-path
         "../ast/ast-expr.rkt"
         "../ast/types.rkt"
         "../bindings.rkt"
         "../scopes.rkt")

(define-runtime-path api "api.rkt")

(define runtime (make-object global-scope%))

(define (get-runtime) runtime)

(define (load-runtime)
        (dynamic-require api 0)
        runtime)

(define-syntax  runtime-add
  (syntax-rules ()
    [(_ id type)
     (add-binding (get-runtime) (null type : id))]
    [(_ id types ret-type)
     (add-binding (get-runtime) (null id types) -> (ret-type null))]))
  
(define-syntax (define/types stx)
  (syntax-case stx ()
    [(_ (id -> ret-type) body ...)
     (with-syntax
         ([new-id
           (datum->syntax stx (mangle-function-id (syntax-e #'id) null))])
       #'(begin
           (runtime-add (build-id #'id) null (create-type 'type))
           (define (new-id) body ...)))]
    [(_ (id [type . arg] -> ret-type) body ...)
     (with-syntax
         ([new-id
           (datum->syntax stx
                          (mangle-function-id
                           (syntax-e #'id)
                           (list (syntax-e #'type))))])
       #'(begin
          (runtime-add (build-id #'id) (create-types (list 'type)) (create-type 'ret-type))
           (define (new-id . arg)
           body ...)))]
    [(_ (id [type arg] ... -> ret-type) body ...)
     (with-syntax
         ([new-id
           (datum->syntax stx
                          (mangle-function-id
                           (syntax-e #'id)
                           (map (lambda (x)
                                  (syntax-e x))
                                (syntax->list #'(type ...)))))])
       #'(begin
           (runtime-add (build-id #'id) (create-types (list 'type ...)) (create-type 'ret-type))
           (define (new-id arg ...)
             body ...)))]
     [(_ type : id value)
     #'(begin
         (runtime-add (build-id #'id) (create-type 'type))
         (define id value))]))

(define-syntax-rule
  (build-id id)
  (make-object identifier% null (racket->java (symbol->string (syntax-e id)))
    (list (syntax-source-module id)
          (syntax-line id)
          (syntax-column id)
          (syntax-position id)
          (syntax-span id))))


(define (new-scope)
  (let ([bindings-hash (send (load-runtime) get-scope)]
        [scope (make-object global-scope%)])
    (send scope set-scope! (hash-copy bindings-hash))
    scope))



