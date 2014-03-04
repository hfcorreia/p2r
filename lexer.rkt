#lang racket
(require parser-tools/lex
         (prefix-in re: parser-tools/lex-sre)
         racket/string)

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-empty-tokens operators 
  ( + - * / ))

(define-empty-tokens terminators 
  (EOF))

(define-tokens literals 
  (integer float char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Token abbreviations exapanded by the lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-lex-abbrevs
  
  ;; integer literals
  (binary  (re:: #\0 "b" (re:+ (re:/ "01"))))
  (octal   (re:: #\0 (re:+ (re:/ "07"))))
  (hexa    (re:: #\0 (char-set "xX") (re:+ (re:/ "09" "af" "AF"))))
  (decimal (re:or #\0 (re:: (re:/ "19") (re:* (re:/ "09")))))
  (long    (re:: decimal (char-set "lL"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lex
  (lexer
   
   ;; operators
   ("+"      (token-+))
   ("-"      (token--))
   ("*"      (token-*))
   ("/"      (token-/))
   
   ;; integers
   (binary   (token-integer (string->number lexeme 2)))
   (octal    (token-integer (string->number lexeme 8)))
   (hexa     (token-integer (string->number lexeme 16)))
   (decimal  (token-integer (string->number lexeme)))
   (long     (token-integer (string->number lexeme)))
   
   ;; terminators
   ((eof)    (token-EOF))))
