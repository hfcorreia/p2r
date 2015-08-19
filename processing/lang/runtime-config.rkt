#lang racket/base

(provide configure)

(require "reader.rkt")

(define (configure data)
  (current-read-interaction processing-read-syntax-repl))
