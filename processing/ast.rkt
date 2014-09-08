(module processing-ast racket

  (require racket/class
           syntax/readerr)
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; AST struct definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; All nodes inherit from ast-node%
  (define ast-node%
    (class object%
           (init-field src-info)

           ;; read-err: string? -> exn:fail:read
           ;; raises an exception with source of the expression
           (define/public (read-error msg)
                          (apply raise-read-error (cons msg src-info)))

           ;; to-racket: -> syntax-object?
           ;; Generates the syntax object relative to the node
           (define/public (to-racket)
                          (read-error (format "Invalid use of to-racket in ast-node ~a") this))

           (define/public (get-src-info) src-info)

           (super-instantiate ())))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define todo-node%
    (class ast-node%
           (init-field child msg)
           (define/override (to-racket indent)
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
