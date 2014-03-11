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

(define-empty-tokens seperators
  (semicolon))

(define-empty-tokens terminators 
  (EOF))

(define-tokens literals 
  (integer float double char boolean))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Token abbreviations exapanded by the lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-lex-abbrevs
  
  ;; numerals and suffix
  (digits      (re:+ (re:/ "09")))
  (float-suf   (char-set "fF"))
  (double-suf  (char-set "dD"))
  (long-suf    (char-set "lL"))
  (hex-digit   (re:/ "09" "af" "AF"))
  
  ;; integer literals
  (binary      (re:: #\0 "b" (re:+ (re:/ "01"))))
  (octal       (re:: #\0 (re:+ (re:/ "07"))))
  (hexa        (re:: #\0 (char-set "xX") (re:+ hex-digit )))
  (decimal     (re:or #\0 (re:: (re:/ "19") (re:* (re:/ "09")))))
  
  ;; float literals
  (float-a     (re:: digits #\. (re:? digits) (re:? exponent)))
  (float-b     (re:: #\. digits (re:? exponent)))
  (float-c     (re:: digits (re:? exponent)))
  (exponent    (re:: (char-set "eE") (re:? (char-set "+-")) digits))
  
  ;; char literals
  (char        (re:: #\' (re:~ whitespace #\' #\\) #\'))
  (escape-seq  (re:or "\\b" "\\t" "\\n" "\\f" "\\r" "\\\"" "\\'" "\\\\"))
  
  
 
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define lex
  (lexer
   ;; whitespaces, linefeeds, newline, etc
   (whitespace (lex input-port))
   (blank      (lex input-port))
   
   ;; seperators
   (";"      (token-semicolon))
   
   ;; operators
   ("+"      (token-+))
   ("-"      (token--))
   ("*"      (token-*))
   ("/"      (token-/))
   
   ;; boolean
   ("true"   (token-boolean #t))
   ("false"  (token-boolean #f))
   
   ;; integers
   (binary     
    (token-integer (string->number (trim-string lexeme 2 0) 2)))
   (octal      
    (token-integer (string->number lexeme 8)))
   (hexa       
    (token-integer (string->number (trim-string lexeme 2 0) 16)))
   (decimal    
    (token-integer (string->number lexeme 10)))
   ((re:: decimal long-suf)       
    (token-integer (string->number (trim-string lexeme 0 1) 10)))
   ((re:: hexa long-suf)
    (token-integer (string->number (trim-string lexeme 2 1) 16)))
   ((re:: octal long-suf)
         (token-integer (string->number (trim-string lexeme 0 1)  8)))
   
   ;; floats
   ((re:: (re:or float-a float-b float-c) float-suf)
    (token-float (string->number (trim-string lexeme 0 1) 10)))
   ((re:: (re:or float-a float-b float-c) double-suf)
    (token-double (string->number (trim-string lexeme 0 1) 10)))
   
   ;; chars
   (char         (token-char (string-ref lexeme 1)))
   ((re:: #\' escape-seq #\')   
    (token-char (escape->char (trim-string lexeme 1 1))))
   
   ;; terminators
   ((eof)    (token-EOF))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Aux funtions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Trims a string given 2 offsets
(define (trim-string string left right)
  (substring string left (- (string-length string) right)))

;;; Converts escape sequences to char
(define (escape->char es)
  (cond
    ((string=? es "\\b") #\backspace)
    ((string=? es "\\t") #\tab)
    ((string=? es "\\r") #\return)
    ((string=? es "\\n") #\newline)
    ((string=? es "\\f") #\page)
    ((string=? es "\\\"") #\")
    ((string=? es "\\'") #\')
    ((string=? es "\\\\'") #\\)))