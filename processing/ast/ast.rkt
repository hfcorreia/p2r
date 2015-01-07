(module ast racket

  (provide (all-defined-out))

  (require racket/class
           syntax/readerr
           "../lib/runtime.rkt")


  ;;; node->racket : (or/c (listof ast-node%) ast-node%) -> (or/c (listof ast-node%) ast-node%)
  ;;; Simplyfies code generation by executing ->racket 
  ;;; over a single or a list of ast-nodes%
  (define-syntax-rule
    (node->racket arg)
    (if (list? arg)
      (map (lambda (elem)
             (send elem ->racket))
           arg)
      (send arg ->racket)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST struct definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Root of the AST. All nodes are a subclass of ast-node%.
  ;;; Encapsulates source info and all required functions to generate
  ;;; syntax-objects.
  (define ast-node%
    (class object%
           (init-field [src-info null])

           (define/public (get-src-info) src-info)

           ;; read-err: string? -> exn:fail:read
           ;; raises an exception with source of the expression
           (define/public (read-error msg)
                          (apply raise-read-error (cons msg src-info)))


           ;; ->syntax-object : datum? -> syntax-object?
           ;; converts the datum to a syntax object using stored src-info
           (define/public (->syntax-object datum)
                          (datum->syntax #'here
                                         datum
                                         (and (not (null? src-info)) src-info)
                                         (read-syntax #f 
                                                      (open-input-string "orig")) 
                                         ))

           ;; ->racket: -> syntax-object?
           ;; generates the syntax-object relative to the node
           (define/public (->racket)
                          (read-error (format "Invalid use of ->racket ~a" this)))

           (super-instantiate ())))


  ;;; Generates initialization code
  ;;; Receives the whole ast and injects initialization code for
  ;;; setup and draw functions
  (define initializer%
    (class ast-node%
           (init-field ast)
           (inherit ->syntax-object)

           (define/override (->racket)
                            (append
                              (node->racket ast)
                              (list 
                                (->syntax-object `(p-initialize setup))
                                (->syntax-object `(p-initialize draw)))))

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

           ;; aux funtion
           (define (aux child)
             (cond 
               [(list? child) (map (lambda (node)
                                     (aux node))
                                   child)]
               [(is-a? child ast-node%) (send child ->racket)]
               [else child]))

           (super-instantiate ())))

  )
