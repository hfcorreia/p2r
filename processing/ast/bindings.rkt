#lang racket

(provide (all-defined-out))

(define global-scope%
  (class object%
         (field [scope (make-hash)])

         (define/public (get-binding id)
                        (hash-ref scope id))

         (define/public (get-scope) (hash->list scope))

         (define/public (add-binding binding)
                        (hash-set! scope (send binding get-id) binding))

         (define/public (is-bound? id)
                        (hash-has-key? scope id))

         (define/public (is-global? id)
                        (hash-has-key? scope id))

         (define/public (is-local? id) #f)

         (super-instantiate ())))

(define local-scope%
  (class global-scope%
         (init-field parent)
         (inherit-field scope)

         (define/override (get-scope)
                          (append (send parent get-scope)
                                  (hash->list scope)))

         (define/override (get-binding id)
                          (hash-ref scope
                                    id
                                    (lambda () (send parent get-binding id))))

         (define/override (is-bound? id)
                          (or (is-local? id)
                              (is-global? id)))

         (define/override (is-global? id)
                          (send parent is-global? id))

         (define/override (is-local? id)
                          (hash-has-key? scope id))

         (super-instantiate ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-function:
;;  (or/c local-scope% global-scope%)
;;  (list/of mod-symbol)
;;  type-symbol
;;  symbol
;;  (list/of type-symbol)
;;  type-symbol
(define-syntax-rule
  (add-function scope modifiers return-type id parameters-types throws)
  (send scope
        add-binding
        (make-object function-binding% modifiers return-type parameters-types throws id)))

;; add-variable:
;;  (or/c local-scope% global-scope%)
;;  (list/of mod-symbol)
;;  type-symbol
;;  symbol
(define-syntax-rule
  (add-variable scope modifiers type id)
  (send scope
        add-binding
        (make-object variable-binding% modifiers type id)))

(define-syntax-rule
  (format-binding binding)
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
