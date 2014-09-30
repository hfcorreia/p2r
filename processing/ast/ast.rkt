(module ast racket

  (require racket/class
           syntax/readerr
           "../lib/runtime.rkt")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST struct definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; All nodes inherit from ast-node%
  (define ast-node%
    (class object%
           ;; inits
           (init-field src-info)

           ;; getters
           (define/public (get-src-info) src-info)

           ;; read-err: string? -> exn:fail:read
           ;; raises an exception with source of the expression
           (define/public (read-error msg)
                          (apply raise-read-error (cons msg src-info)))


           ;; ->s : datum? -> syntax-object?
           ;; converts the datum to a syntax object using stored src-info
           (define/public (->syntax-object datum)
                          (datum->syntax #'test
                                         datum
                                         src-info
                                         (read-syntax #f 
                                                      (open-input-string "orig")) 
                                         ))

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/public (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/public (->xml indent)
                          (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))

  ;;; Type nodes
  (define primitive-type% 
    (class ast-node%
           (init-field type)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (send type ->racket))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<primitive-type type=\"~a\" />"
                                    (make-string indent #\space)
                                    type))
           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Debug stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define todo? #f)
  (define (clear-todo)
    (set! todo? #f))

  (define todo-node%
    (class ast-node%
           (init-field child msg)

           (inherit ->syntax-object read-error)

           (define/override (->racket)
                            (clear-todo)
                            (->syntax-object
                              (aux child)))

           (define/override (->xml indent)
                            (if (null? child)
                              (format "~%~a<todo msg=\"~a\" />"
                                      (make-string indent #\space)
                                      msg)
                              (format "~%~a<todo msg=\"~a\">~a~%~a</todo>"
                                      (make-string indent #\space)
                                      msg
                                      (generate child (+ indent 2))
                                      (make-string indent #\space))))

           ;; aux funtion
           (define (aux child)
             (cond 
               [(list? child) (map (lambda (node)
                                     (aux node))
                                   child)]
               [(is-a? child ast-node%) (send child ->racket)]
               [else child]))

           (define (generate child indent)
             (cond
               [(list? child) (string-append* 
                                (map (lambda (node)
                                       (generate node (+ indent 2)))
                                     child))]
               [else (send child ->xml (+ indent 2))]))

           (super-instantiate ())))

  )
