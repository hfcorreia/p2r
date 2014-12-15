(module lexer racket/base

  (provide (all-defined-out))

  (require parser-tools/lex
           (prefix-in re: parser-tools/lex-sre)
           racket/string)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Tokens definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-empty-tokens operators 
                       (
                        PIPE OR OREQUAL =    >    <    !     ~    ?     :
                        ==   <=   >=   !=    &&   ++    --
                        +    -    *    /     &    ^     %     <<     >>     >>>
                        +=   -=   *=   /=    &=   ^=    %=    <<=    >>=    >>>=))

  (define-empty-tokens keywords 
                       (
                        abstract    continue    for          new          switch
                        assert      default     if            package      synchronized
                        boolean     do          goto          private      this
                        break       double      implements    protected    throw
                        byte        else        import        public       throws
                        case        enum        instanceof    return       transient
                        catch       extends     int           short        try
                        char        final       interface     static       void 
                        class       finally     long          strictfp     volatile
                        const       float       native        super        while
                        ; custom keywords
                        color))



  (define-empty-tokens seperators 
                       (
                        semicolon period comma
                        l-paren  r-paren
                        l-cbrack r-cbrack
                        l-sbrack r-sbrack))

  (define-empty-tokens empty-literals (EOF null-lit))

  (define-tokens literals 
                 ( 
                   identifier int-lit long-lit float-lit double-lit 
                   char-lit boolean-lit string-lit token-lit color-lit
                   ; custom token
                   require))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Token abbreviations exapanded by the lexer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-lex-abbrevs
    (input-charcter (re:~ #\return #\linefeed))

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
    (char        (re:: #\' (re:~ #\' #\\) #\'))
    (escape-seq  (re:or "\\b" "\\t" "\\n" "\\f" "\\r" "\\\"" "\\'" "\\\\"
                        (re:: #\\ (re:? (re:/ "03")) (re:/ "07") (re:/ "07"))
                        (re:: #\\ (re:/ "07"))))
    ;; string literals
    (string      (re:: #\" (re:* (re:~ #\" )) #\"))

    ;; web color literal
    (web-color  (re:: #\# (re:= 6 hex-digit)))

    ;; keywords
    (keyword     (re:or "abstract"    "continue"    "for"           "new"          "switch"
                        "assert"      "default"     "if"            "package"      "synchronized"
                        "boolean"     "do"          "goto"          "private"      "this"
                        "break"       "double"      "implements"    "protected"    "throw"
                        "byte"        "else"        "import"        "public"       "throws"
                        "case"        "enum"        "instanceof"    "return"       "transient" 
                        "catch"       "extends"     "int"           "short"        "try"
                        "char"        "final"       "interface"     "static"       "void"
                        "class"       "finally"     "long"          "strictfp"     "volatile"
                        "const"       "float"       "native"        "super"        "while"
                        ;; custom keywords
                        "color"))

    ;; operator
    (operator   (re:or "="    ">"    "<"    "!"     "~"    "?"     ":"
                       "=="   "<="   ">="   "!="    "&&"   "||"    "++"    "--"
                       "+"    "-"    "*"   "/"     "&"    "|"     
                       "^"     "%"     "<<"     ">>"     ">>>"
                       "+="   "-="   "*="  "/="    "&="   "|="    
                       "^="    "%="    "<<="    ">>="    ">>>="))

    ;; identifier
    (identifier  (re:: (re:or "_" "$" (re:/ "az" "AZ"))
                       (re:*  (re:or (re:/ "AZ" "az" "09") "_" "$")))) 

    ;; comments
    (line-comment       (re:: "//" (re:* input-charcter) ))

    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Lexer definition
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define lex
    (lexer-src-pos
      ;; special rule to ignore #lang processing directives
      ((re:: #\# "lang processing") (return-without-pos (lex input-port)))


      ;; whitespaces, linefeeds, newline, etc
      ((re:+ whitespace)    (return-without-pos (lex input-port)))
      ((re:+ blank)         (return-without-pos (lex input-port)))
      (line-comment         (return-without-pos (lex input-port)))
      ("/*"                 (begin (comment-lex input-port)
                                   (return-without-pos (lex input-port))))

      ;; seperators
      (";"      (token-semicolon))
      ("."      (token-period))
      (","      (token-comma))
      ("("      (token-l-paren))
      (")"      (token-r-paren))
      ("{"      (token-l-cbrack))
      ("}"      (token-r-cbrack))
      ("["      (token-l-sbrack))
      ("]"      (token-r-sbrack))

      ;; operators
      (operator     (let ((l lexeme))
                      (cond
                        ((string=? l "|")  (token-PIPE))
                        ((string=? l "||") (token-OR))
                        ((string=? l "|=") (token-OREQUAL))
                        (else (string->symbol lexeme)))))

      ;; boolean
      ("true"   (token-boolean-lit #t))
      ("false"  (token-boolean-lit #f))

      ;; integers
      (binary     
        (token-int-lit (string->number (trim-string lexeme 2 0) 2)))
      (octal      
        (token-int-lit (string->number lexeme 8)))
      (hexa       
        (token-int-lit (string->number (trim-string lexeme 2 0) 16)))
      (decimal    
        (token-int-lit (string->number lexeme 10)))

      ;; longs
      ((re:: decimal long-suf)       
       (token-long-lit (string->number (trim-string lexeme 0 1) 10)))
      ((re:: hexa long-suf)
       (token-long-lit (string->number (trim-string lexeme 2 1) 16)))
      ((re:: octal long-suf)
       (token-long-lit (string->number (trim-string lexeme 0 1)  8)))

      ;; floats
      ((re:: (re:or float-a float-b float-c))
       (token-float-lit (string->number (trim-string lexeme 0 0) 10)))
      ((re:: (re:or float-a float-b float-c) float-suf)
       (token-float-lit (string->number (trim-string lexeme 0 1) 10)))
      ((re:: (re:or float-a float-b float-c) double-suf)
       (token-double-lit (string->number (trim-string lexeme 0 1) 10)))

      ;; chars
      (char            (token-char-lit (string-ref lexeme 1)))
      ((re:: #\' escape-seq #\')   
       (token-char-lit (escape->char (trim-string lexeme 1 1))))

      ;; strings
      (string         (token-string-lit (build-string lexeme)))

      ;; webcolor
      (web-color      (token-color-lit  (build-string lexeme)))

      ;; null
      ("null"         (token-null-lit))

      ;; keywords
      (keyword        (string->symbol lexeme))

      ;; identifiers
      (identifier     (token-identifier lexeme))

      ;; used in require
      ((re:: "require" (re:* (re:~ ";"))) 
        (token-require (trim-string lexeme 7 0)))

      ;; terminator
      ((eof)    (token-EOF))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; comment lexer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define comment-lex
    (lexer-src-pos
      ("*/" end-pos)
      ((eof) end-pos)
      ((re:or "*" "/" (complement (re:: any-string (re:or "*" "/") any-string)))
       (comment-lex input-port))
      ((special) (comment-lex input-port))
      ((special-comment) (comment-lex input-port))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; string lexer
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-tokens str-tokens (str-end str-char))

  (define-empty-tokens empty-str-tokens (str-eof))

  (define (build-string input)
    (define (build-string-aux input-port)
      (let ((token (position-token-token (string-lex input-port))))
        (if (eq? (token-name token)
                 'str-eof)
          (list)
          (let ((value (if (char? (token-value token))
                         (make-string 1 (token-value token))
                         (token-value token))))
            (cons value (build-string-aux input-port))))))
    (string-append* "" (build-string-aux (open-input-string input))))

  (define string-lex
    (lexer-src-pos
      (#\"          (return-without-pos (string-lex input-port)))
      (escape-seq   (token-str-char (escape->char lexeme)))
      ((re:~ #\")   (token-str-char lexeme))
      (whitespace   (token-str-char lexeme))
      (blank        (token-str-char lexeme))
      ((eof)        (token-str-eof))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Aux funtions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;; Trims a string given 2 offsets
  (define (trim-string string left right)
    (substring string left (- (string-length string) right)))

  ;;; Converts escape sequences to char
  (define (escape->char es)
    (cond
      ((string=? es "\\b") #\010)
      ((string=? es "\\t") #\011)
      ((string=? es "\\n") #\012)
      ((string=? es "\\f") #\014)
      ((string=? es "\\r") #\015)
      ((string=? es "\\\"") #\")
      ((string=? es "\\'") #\')
      ((string=? es "\\\\") #\\)
      (else (integer->char (string->number (trim-string es 1 0)) 8)))))
