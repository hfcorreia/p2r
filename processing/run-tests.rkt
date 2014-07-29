#lang racket

(require rackunit/text-ui
         rackunit
         "parser.rkt"
         "lexer.rkt"
         "ast.rkt")

;;; Macro to generate a test case
(define-syntax-rule
  (new-test-case description input expected)
  (test-case
   description
   (check-equal? (parse-processing #f (open-input-string input)) expected)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test suites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
