#lang racket/base

(provide active-mode? active-mode)

(define mode #f)

(define (active-mode?) mode)

(define (active-mode) (set! mode #t))
