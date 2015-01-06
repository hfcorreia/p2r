#lang racket/base

(provide active-mode? active-mode)

;;; Used by the parser to register if the current processing code is in:
;;;  active mode or static mode

(define mode #f)

(define (active-mode?) 
  mode)

(define (active-mode) 
  (set! mode #t))
