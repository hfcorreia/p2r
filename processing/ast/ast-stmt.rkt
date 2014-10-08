(module ast/stmt racket 

  (provide (all-defined-out))

  (require racket/class
           "ast.rkt")

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST stmt nodes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define stmt%
    (class ast-node%
           (inherit read-error)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; global stmt
  (define global-stmt%
    (class stmt%
           (init-field stmt)

           (inherit-field src-info)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              `(void ,(node->racket stmt))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~a<global-stmt>~a~%~a</global-stmt>~%"
                                    (make-string indent #\space)
                                    (send stmt ->xml (+ indent 2))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  ;;; global declaration
  (define global-decl%
    (class stmt%
           (init-field decl)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (node->racket decl))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~a<global-decl>~a~%~a</global-decl>~%"
                                    (make-string indent #\space)
                                    (send decl ->xml (+ indent 2))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  ;;; variable declaration
  (define vars-decl%
    (class stmt%
           ;; TODO: types and modifiers
           (init-field modifiers type vars)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              `(begin ,@(node->racket vars))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<vars-decl>~a~a~%~a~%~a</vars-decl>"
                                    (make-string indent #\space)
                                    (if (null? modifiers)
                                      ""
                                      (send modifiers ->xml (+ indent 2)))
                                    (send type ->xml (+ indent 2))
                                    (string-append*
                                      (map (lambda (var)
                                             (send var ->xml (+ indent 2)))
                                           vars))
                                    (make-string indent #\space)))
           (super-instantiate ())))

  ;;; variable declaration
  (define var-decl%
    (class stmt%
           ;; TODO: types and modifiers
           (init-field var value)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              (if (null? value)
                                `(p-declaration ,(node->racket var))
                                `(p-declaration ,(node->racket var)
                                                ,(node->racket value)))))

           ;; ->xml: ->string?
           ;; Generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<var-decl>~a~a~%~a</var-decl>"
                                    (make-string indent #\space)
                                    (send var ->xml (+ indent 2))
                                    (if (not (null? value))
                                      (send value ->xml (+ indent 2))
                                      "") 
                                    (make-string indent #\space)))
           (super-instantiate ())))

  (define block%
    (class stmt%
           (init-field stmts)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              (if (null? stmts) 
                                `(let () (void))
                                `(let ()
                                   ,@(node->racket stmts)
                                   (void)))))


           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<block>~a~%~a</block>~%"
                                    (make-string indent #\space)
                                    (string-append*
                                      (map (lambda (stmt)
                                             (send stmt ->xml (+ indent 2)))
                                           stmts))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  (define method-decl% 
    (class stmt%
           (init-field header body)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(define ,(node->racket header)
                                 (call/ec (lambda (return)
                                            ,(node->racket body))))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<method-decl>~a~%~a~%~a</method-decl>"
                                    (make-string indent #\space)
                                    (send header ->xml (+ indent 2))
                                    (send body ->xml (+ indent 2))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  (define method-header%
    (class stmt%
           (init-field modifiers type id parameters throws)

           (inherit ->syntax-object)

           ;; Getters
           (define/public (get-id) id)
           (define/public (get-parameters) parameters)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(,(node->racket id)
                                 ,@(node->racket parameters))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format "~%~a<method-decl>~a~%~a~%~a~%~a~%~a~%~a</method-decl>"
                                    (make-string indent #\space)
                                    (if (null? modifiers)
                                      ""
                                      (send modifiers ->xml (+ indent 2)))
                                    (send type ->xml (+ indent 2))
                                    (send id ->xml (+ indent 2))
                                    (if (null? parameters)
                                      ""
                                      (string-append*
                                        (map (lambda (var)
                                               (send var ->xml (+ indent 2)))
                                             parameters)))
                                    (if (null? throws)
                                      ""
                                      (send throws ->xml (+ indent 2)))
                                    (make-string indent #\space)))

           (super-instantiate ())))

  (define formal-parameter% 
    (class stmt%
           (init-field final type id)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              (node->racket id)))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<formal-parameter>~a~%~a~%~a~%~a<formal-parameter/>"
                              (make-string indent #\space)
                              final
                              (send type ->xml (+ indent 2))
                              (send id ->xml (+ indent 2))
                              (make-string indent #\space)))

           (super-instantiate ())))

  (define if% 
    (class stmt%
           (init-field test then else)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(if ,(node->racket test)
                                 ,(node->racket then)
                                 ,(if (null? else) (void) (node->racket
                                                            else)))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<if>~a~%~a~%~a~%~a<if/>"
                              (make-string indent #\space)
                              (send test ->xml)
                              (send then ->xml)
                              (if (null? else) "" (send else ->xml))
                              (make-string indent #\space)))

           (super-instantiate ())))

  (define do-while%
    (class stmt%
           (init-field test body)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                             `(call/ec (lambda (break)
                                         (let loop ()
                                           (call/ec (lambda (continue)
                                                      ,(node->racket body)))
                                           (when ,(node->racket test)
                                             (loop)))))))
           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<do-while>~a~%~a~%~a<do-while/>"
                              (make-string indent #\space)
                              (send test ->xml)
                              (send body ->xml)
                              (make-string indent #\space)))

           (super-instantiate ())))

  (define while%
    (class stmt%
           (init-field test body)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(call/ec 
                                 (lambda (break)
                                   (letrec ([loop (lambda ()
                                                    (when ,(node->racket test)
                                                      (call/ec (lambda (continue)
                                                                 ,(node->racket body)))
                                                      (loop)))])
                                     (loop))))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<while>~a~%~a~%~a<while/>"
                              (make-string indent #\space)
                              (send test ->xml)
                              (send body ->xml)
                              (make-string indent #\space)))

           (super-instantiate ())))



  (define return% 
    (class stmt%
           (init-field expr)

           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              (if(null? expr)
                                `(return (void))
                                `(return ,(node->racket expr)))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<return>~%~a~%~a<return/>"
                              (make-string indent #\space)
                              (if (null? expr)
                                ""
                                (send expr ->xml (+ indent 2)))
                              (make-string indent #\space)))

           (super-instantiate ())))

  (define break% 
    (class stmt%
           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(break (void))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<break />"
                              (make-string indent #\space)))

           (super-instantiate ())))

  (define continue% 
    (class stmt%
           (inherit ->syntax-object)

           ;; ->racket: -> syntax-object?
           ;; generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object 
                              `(continue (void))))

           ;; ->xml: ->string?
           ;; generates xml representation of the node
           (define/override (->xml indent)
                            (format
                              "~%~a<break />"
                              (make-string indent #\space)))

           (super-instantiate ())))
  )
