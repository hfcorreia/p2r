#lang racket/base

(provide (all-defined-out))

(require racket/math
         "../scopes.rkt")

(define-types float HALF_PI (/ pi 2))
(define-types float PI pi)
(define-types float QUARTER_PI (/ pi 4))
(define-types float TAU (* pi 2))
(define-types float TWO_PI (* pi 2))
