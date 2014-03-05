#lang racket

(require rackunit/text-ui
         rackunit
         "parser.rkt"
         "lexer.rkt"
         "ast.rkt")

(define (parse input)
  (simple-parser (lambda()(lex input))))

;;; Macro to generate a test case
(define-syntax-rule
  (new-test-case description input expected)
  (test-case
   description
   (check-equal? (parse (open-input-string input)) expected)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test suites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-test-suite integers-literals
  (new-test-case "Binary literal" "0b1111;" (stmt (num-exp 15)))
  (new-test-case "Octal literal" "077;" (stmt (num-exp 63)))
  (new-test-case "Decimal literal"  "112;" (stmt (num-exp 112))) 
  (new-test-case "Long literal" "10L;" (stmt (num-exp 10)))
  (new-test-case "Octal Long literal" "077L;" (stmt (num-exp 63)))
  (new-test-case "Hexadecimal Long literal" "0xFFl;" (stmt (num-exp 255)))
  (new-test-case "Hexadecimal literal" "0xFF;" (stmt (num-exp 255))))


;;; Run test suites
(run-tests integers-literals)
