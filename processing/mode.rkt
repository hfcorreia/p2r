#lang racket

(provide (all-defined-out))

;;; Used by the parser to register if the current processing code is in
;;;  active mode or static mode
(define mode #f)

(define (active-mode?) mode)

(define (set-active-mode! [pred #t])
    (set! mode pred))
