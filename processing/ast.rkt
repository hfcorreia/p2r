(module processing-ast racket

  (require racket/class
           syntax/readerr
           "runtime.rkt")

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


           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Global stmt
  (define global-stmt%
    (class ast-node% 
           (init-field stmt)

           ;; getters
           (define/public (get-stmt) stmt)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (send stmt ->racket))

           (super-instantiate ())))
           
  ;;; Identifier 
  (define identifier%
    (class ast-node%
           ;; inits
           (init-field identifier)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-identifier) identifier)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object (identifier->symbol)))

           (define (identifier->symbol)
            (if (symbol? identifier)
              identifier 
              (string->symbol identifier))) 

           (super-instantiate ())))

  ;;; Literals
  (define literal%
    (class ast-node%
           ;; inits
           (init-field value type)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-value) value)
           (define/public (get-type) type)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket) (->syntax-object value))

           (super-instantiate ())))

  ;;; Method Call
  (define method-call% 
    (class ast-node%
           ;; inits
           (init-field name args)

           (inherit ->syntax-object)

           ;; getters
           (define/public (get-name) name)
           (define/public (get-args) args)

           ;; ->racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/override (->racket)
                            (->syntax-object
                              `(dispatch ,(send name ->racket) 
                                        ,(send (car args) ->racket))))

           (super-instantiate ())))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Debug stuff
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define todo-node%
    (class ast-node%
           (init-field child msg)
           (define/override (->racket indent)
                            (if (null? child)
                              (format "~aTodo: ~a~%" (p-indent indent) msg)
                              (string-append (format "~aTodo: ~a~%" 
                                                     (p-indent indent) msg)
                                             (traverse child (+ 2 indent)))))
           (super-new)))

  (define (traverse node indent)
    (cond
      ((null? node) "")
      ((list? node)
       (string-append (format "~aList: ~%" (p-indent indent))
                      (foldl string-append "" 
                             (map (lambda (x) (traverse x (+ 2 indent)))
                                  node))))
      ((is-a? node todo-node%)
       (string-append (format "~aTodo: ~a~%" (p-indent indent) (get-field msg node))
                      (traverse (get-field child node) (+ 2 indent))))
      (else "")))

  (define (p-indent val)
    (make-string val #\space))


  )
