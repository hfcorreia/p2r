#lang racket/base

(provide (all-defined-out))

(require racket/math
         "../bindings.rkt"
         "../ast/types.rkt")

(define-types null (create-type 'float) HALF_PI (/ pi 2))
(define-types null (create-type 'float) PI pi)
(define-types null (create-type 'float) QUARTER_PI (/ pi 4))
(define-types null (create-type 'float) TAU (* pi 2))
(define-types null (create-type 'float) TWO_PI (* pi 2))
