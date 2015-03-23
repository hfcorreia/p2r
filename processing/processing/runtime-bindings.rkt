#lang racket

(provide (all-defined-out))

(require (for-syntax "../util.rkt")
         racket/runtime-path
         "../ast/ast-expr.rkt"
         "../ast/types.rkt"
         "../bindings.rkt"
         "../scopes.rkt")

(define-runtime-path api "api.rkt")

(define runtime null)

(define (load-runtime)
  (if (not (null? runtime))
      runtime
      (begin
        (set! runtime (make-object global-scope%))
        (dynamic-require api 0)
        runtime)))

(define-syntax (define/types stx)
  (syntax-case stx ()
    [(_ type id value)
     #'(begin
         (add-binding runtime ('() (create-type 'type) : (build-id #'id)))
         (define id value))]
    [(_ (id [type . arg] -> ret-type) body ...)
     (with-syntax
         ([new-id
           (datum->syntax stx
                          (mangle-function-id
                           (syntax-e #'id)
                           (list (syntax-e #'type))))])
       #'(begin
           (add-binding runtime
                        (null (build-id #'id) (create-types (list 'type)))
                        ->
                        ((create-type 'ret-type) null))
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
           (add-binding runtime
                        (null (build-id #'id) (create-types (list 'type ...)))
                        ->
                        ((create-type 'ret-type) null))
           (define (new-id arg ...)
             body ...)))]))

(define-syntax-rule
  (build-id id)
  (make-object identifier% null (symbol->string (syntax-e id))
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



