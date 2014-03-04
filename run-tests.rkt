#lang racket

(require rackunit
         rackunit/text-ui)

(run-tests
(test-suite
 "Test integer literals"
 
 
 #:before (lambda () (displayln "Testing integer literals"))

 (test-case
  "Decimal tests"
  (check-eq? 1 2))
 
 (test-case
  "Decimal tests2"
  (check-eq? 1 2))))

; Test with whitespaces, CR, line feed, etc
; Test with octal, hexadecimal, etc notations
; Test with test with parentisis
; Test with delimitator(Semicolon,etc)
; Test with variable atribution
