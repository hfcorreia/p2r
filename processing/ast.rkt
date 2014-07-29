(module processing-ast racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro that provides and defines a new struct for the AST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax p-define-struct
  (syntax-rules ()
    [(_ (name inherit) fields)
     (begin
       (provide (struct-out name))
       (define-struct (name inherit) fields #:mutable #:transparent))]
    [(_ name fields)
     (begin
       (provide (struct-out name))
       (define-struct name fields #:mutable #:transparent))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AST struct definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(p-define-struct make-todo (info))
)

