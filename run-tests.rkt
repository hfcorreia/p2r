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

(define-test-suite float-literals
  (new-test-case "NUM.f" "2.f;" (stmt (num-exp 2.0)))
  (new-test-case "Exponent" "1e1f;" (stmt (num-exp 10.0)))
  (new-test-case "NUM.NUMf" "3.14f;" (stmt (num-exp 3.14)))
  (new-test-case "Positive exponent" "6.02e+2f;" (stmt (num-exp 602.0)))
  (new-test-case "Float literal" "6.02E-2f;" (stmt (num-exp 0.0602)))
  (new-test-case "Float literal" ".3f;" (stmt (num-exp .3))))

(define-test-suite double-literals
  (new-test-case "NUM.d" "2.d;" (stmt (num-exp 2.0)))
  (new-test-case "Exponent" "1e1D;" (stmt (num-exp 10.0)))
  (new-test-case "NUM.NUMd" "3.14D;" (stmt (num-exp 3.14)))
  (new-test-case "Positive exponent" "6.02e+2d;" (stmt (num-exp 602.0)))
  (new-test-case "Negative exponent" "6.02E-2D;" (stmt (num-exp 0.0602))))

(define-test-suite boolean-literals
  (new-test-case "true" "true;" (stmt (num-exp #t)))
  (new-test-case "false" "false;" (stmt (num-exp #f))))

(define-test-suite char-literals
  (new-test-case "simple char letter" "'a';" (stmt (num-exp #\a)))
  (new-test-case "tab escaped sequence" "'\\t';" (stmt (num-exp #\tab)))
  (new-test-case "quotation escaped sequence" "'\\\"';" (stmt (num-exp #\"))))

(define-test-suite string-literals
  (new-test-case "empty string" "\"\";" (stmt (num-exp "")))
  (new-test-case "blobs string" "\"blobs\";" (stmt (num-exp "blobs")))
  (new-test-case "with spaces" "\"s pa ces \";" (stmt (num-exp "s pa ces ")))
  (new-test-case "with spaces" "\"s pa ces \n\";" (stmt (num-exp "s pa ces \n")))
  (new-test-case "with escape" "\" \\n\";" (stmt (num-exp " \n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run test suites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test-suites 
  (list integers-literals 
        float-literals 
        double-literals
        boolean-literals
        char-literals
        string-literals))

(map run-tests test-suites)

