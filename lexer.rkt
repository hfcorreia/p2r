#lang racket

(require parser-tools/lex
         (prefix-in re: parser-tools/lex-sre))

(provide (all-defined-out))

; Tokens
(define-empty-tokens operators 
                     ( + - * / ))

(define-empty-tokens terminator (EOF))

(define-tokens literals (integer real))

; Lexer
(define lex
  (lexer
    ("+"     (token-+))
    ("-"     (token--))
    ("*"     (token-*))
    ("/"     (token-/))
    (numeric (token-integer (string->number lexeme)))
    ((eof)   (token-EOF))))
