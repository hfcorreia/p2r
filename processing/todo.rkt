#lang racket

(require "ast/ast.rkt")

(provide make-todo todo? set-todo!)

(define todo #f)

(define (todo?) todo)

(define (set-todo! [pred #t])
  (set! todo pred))

(define (make-todo msg src)
  (set-todo!)
  (make-object todo-node% msg src))
