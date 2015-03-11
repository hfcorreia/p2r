#lang racket

(provide (all-defined-out))

(require "ast/types.rkt"
         (for-syntax "ast/types.rkt")
         "name-mangling.rkt"
         (for-syntax "name-mangling.rkt"))

(define global-scope%
  (class object%
         (field [scope (make-hash)])

         (define/public (return-type) #f)

         (define/public (get-binding id)
                        (hash-ref scope id))

         (define/public (get-scope) (hash->list scope))

         (define/public (add-binding binding)
                        (hash-set! scope (send binding get-id) binding))

         (define/public (bound? id)
                        (hash-has-key? scope id))

         (define/public (global? id)
                        (hash-has-key? scope id))

         (define/public (local? id) #f)

         (super-instantiate ())))

(define local-scope%
  (class global-scope%
         (init-field parent [ret-type #f])
         (inherit-field scope)

         (define/override (get-scope)
                          (append (send parent get-scope)
                                  (hash->list scope)))

         (define/override (get-binding id)
                          (hash-ref scope
                                    id
                                    (lambda () (send parent get-binding id))))

         (define/override (return-type) ret-type)

         (define/override (bound? id)
                          (or (local? id)
                              (global? id)))

         (define/override (global? id)
                          (send parent is-global? id))

         (define/override (local? id)
                          (hash-has-key? scope id))

         (super-instantiate ())))

;; Create the global enviroment
(define global-scope (make-object global-scope%))


;; Add bindings to the global scope
(define-syntax (define-types stx)
  (syntax-case stx ()
    [(_ type id value)
     #'(begin
         (add-variable global-scope '() (create-type 'type) 'id)
         (define id value))]
    [(_ (id [type . arg] -> ret-type) body ...)
     (with-syntax
       ([new-id
          (datum->syntax stx
                         (mangle-function-id
                           (syntax-e #'id)
                           (list (syntax-e #'type))))])
       #'(begin
           (add-function global-scope
                         null
                         (create-type 'ret-type)
                         'id
                         (create-types (list 'type))
                         null)
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
           (add-function global-scope
                         null
                         (create-type 'ret-type)
                         'id
                         (create-types (list 'type ...))
                         null)
           (define (new-id arg ...)
             body ...)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-function:
;;  (or/c local-scope% global-scope%)
;;  (list/of mod-symbol)
;;  type%
;;  symbol
;;  (list/of types)
;;  type-symbol
(define-syntax-rule
  (add-function scope modifiers return-type id parameters-types throws)
  (send scope
        add-binding
        (make-object function-binding% modifiers return-type parameters-types throws
                     (mangle-function-id id (map (lambda (x)
                                                   (send x get-type))
                                                 parameters-types)))))

;; add-variable:
;;  (or/c local-scope% global-scope%)
;;  (list/of mod-symbol)
;;  type%
;;  symbol
(define-syntax-rule
  (add-variable scope modifiers type id)
  (send scope
        add-binding
        (make-object variable-binding% modifiers type id)))

(define (format-binding binding)
  (if (is-a? binding variable-binding%)
    (format "id: ~a, type: ~a, mod: ~a"
            (send binding get-id)
            (send (send binding get-type) get-type)
            (send binding get-modifiers))
    (format "id: ~a, args: ~a, mod: ~a, ret: ~a"
            (send binding get-id)
            (map (lambda (x)
                   (send x get-type))
                 (send binding get-args))
            (send binding get-modifiers)
            (send (send binding get-return-type) get-type))))

(define binding%
  (class object%
         (init-field id)

         (define/public (get-id) id)

         (super-instantiate ())))

(define function-binding%
  (class binding%
         (init-field modifiers return-type args throws)

         (define/public (get-modifiers) modifiers)
         (define/public (get-return-type) return-type)
         (define/public (get-arity) (length args))
         (define/public (get-args) args)

         (super-instantiate ())))

(define variable-binding%
  (class binding%
         (init-field modifiers type)

         (define/public (get-modifiers) modifiers)
         (define/public (get-type) type)

         (super-instantiate ())))


