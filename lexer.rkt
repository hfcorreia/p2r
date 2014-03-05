#lang racket
(require parser-tools/lex
         (prefix-in re: parser-tools/lex-sre)
         racket/string)

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Trims a string given 2 offsets
(define (trim-string string left right)
  (substring string left (- (string-length string) right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-empty-tokens operators 
  ( + - * / ))

(define-empty-tokens seperators
  (semicolon))

(define-empty-tokens terminators 
  (EOF))

(define-tokens literals 
  (integer float char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Token abbreviations exapanded by the lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-lex-abbrevs
  
  ;; integer literals
  (binary      (re:: #\0 "b" (re:+ (re:/ "01"))))
  (octal       (re:: #\0 (re:+ (re:/ "07"))))
  (hexa        (re:: #\0 (char-set "xX") (re:+ (re:/ "09" "af" "AF"))))
  (decimal     (re:or #\0 (re:: (re:/ "19") (re:* (re:/ "09")))))
  (long        (re:: decimal (char-set "lL")))
  (long-hexa   (re:: hexa (char-set "lL")))
  (long-octal  (re:: octal (char-set "lL")))
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lex
  (lexer
   ;; whitespaces, linefeeds, newline, etc
   (whitespace (lex input-port))
   
   ;; seperators
   (";"      (token-semicolon))
   
   ;; operators
   ("+"      (token-+))
   ("-"      (token--))
   ("*"      (token-*))
   ("/"      (token-/))
   
   ;; integers
   (binary     (token-integer (string->number (trim-string lexeme 2 0) 2)))
   (octal      (token-integer (string->number lexeme 8)))
   (hexa       (token-integer (string->number (trim-string lexeme 2 0) 16)))
   (decimal    (token-integer (string->number lexeme 10)))
   (long       (token-integer (string->number (trim-string lexeme 0 1) 10)))
   (long-hexa  (token-integer (string->number (trim-string lexeme 2 1) 16)))
   (long-octal (token-integer (string->number (trim-string lexeme 0 1)  8)))
   
   ;; terminators
   ((eof)    (token-EOF))))
