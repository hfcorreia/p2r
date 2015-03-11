#lang racket

(provide (all-defined-out)
         (all-from-out "boo.rkt"))

(require "boo.rkt")

(define f "lol")
(define (foo-bar foo)
  (displayln foo))

(define (bazz! foo j asd sdd dasd)
  (displayln foo))
