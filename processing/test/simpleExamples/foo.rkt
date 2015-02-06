(module foo racket
  (provide (all-defined-out))

  (define (foo bar) (displayln bar))
  )
