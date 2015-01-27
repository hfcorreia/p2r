#lang racket

(provide (all-defined-out))

(define binding-scope<%>
  (interface ()
    add-binding
    is-bound?
    is-global?
    is-local?))

(define global-scope%
  (class* object% (binding-scope<%>)
    (define scope '())
    
    (define/public (add-binding id)
      (set! scope (cons id scope)))
    
    (define/public (is-bound? id)
      (member id scope))
    
    (define/public (is-global? id) #t)
    
    (define/public (is-local? id) #f)
                                
    (define/public (get-scope) scope)

    (super-instantiate ())))


(define local-scope%
  (class* object% (binding-scope<%>)
    (init-field parent-scope)
    
    (define scope '())
    
    (define/public (add-binding id)
       (set! scope (cons id scope)))
    
    (define/public (is-bound? id)
      (or (is-local? id)
          (is-global? id)))
    
    (define/public (is-global? id)
      (send parent-scope is-global? id))
    
    (define/public (is-local? id)
      (member id scope))
    
    (super-instantiate ())))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule 
  (add-function-binding scope modifiers return-type id parameters throws)
  (send scope 
        add-binding 
        (make-object function-binding% modifiers return-type parameters throws id)))

(define-syntax-rule 
  (add-variable-binding scope modifiers type id)
  (send scope 
        add-binding
        (make-object variable-binding% modifiers type id)))

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