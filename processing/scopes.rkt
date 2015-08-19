#lang racket

(provide (all-defined-out))

(require "ast/errors.rkt"
         "ast/types.rkt"
         "bindings.rkt"
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scopes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define global-scope%
  (class object%
         (define scope (make-hash))

         (define/public (return-type) #f)

         (define/public (get-binding id)
                        (hash-ref scope (send id get-id)))

         (define/public (get-variable id)
                        (filter variable-binding? (get-binding id)))

         (define/public (get-functions id)
                        (filter function-binding? (get-binding id)))


         (define/public (get-scope) scope)
         (define/public (set-scope! s) (set! scope s))

         (define/public (add-binding bind)
                        (let ([id-sym (send (send bind get-id) get-id)])
                          (if (not (global? id-sym))
                            (hash-set! scope id-sym (list bind))
                            (hash-update! scope
                                          id-sym
                                          (lambda (bindings)
                                            (check-binds bind
                                                         bindings))))))

         (define/public (bound? id)
                        (hash-has-key? scope id))

         (define/public (global? id)
                        (hash-has-key? scope id))

         (define/public (local? id) #f)

         (super-instantiate ())))

(define local-scope%
  (class global-scope%
         (init-field parent [ret-type #f])
         (define scope (make-hash))

         (define/override (add-binding bind)
                          (let ([id-sym (send (send bind get-id) get-id)])
                            (if (not (local? id-sym))
                              (hash-set! scope id-sym (list bind))
                              (duplicate-variable (send bind get-id) id-sym))))

         (define/override (get-functions id)
                          (send parent get-functions id))

         (define/override (get-scope)
                          (append (send parent get-scope)
                                  (hash->list scope)))

         (define/override (get-binding id)
                          (hash-ref scope
                                    (send id get-id)
                                    (lambda () (send parent get-binding id))))

         (define/override (return-type) ret-type)

         (define/override (bound? id)
                          (or (local? id)
                              (send parent bound? id)))

         (define/override (global? id)
                          (send parent global? id))

         (define/override (local? id)
                          (hash-has-key? scope id))

         (super-instantiate ())))
