#Processing 2 Racket
Implementation of the Processing Language in DrRacket.

---
Reflects the current state of parser!
#Available Tokens

##Binary Operators
* + - * /

##Literals
* integer
* float
* double
* char
* string
* boolean
* null

##Seperators
* semicolon
* l-paren (
* r-paren )
* l-cbrack {
* r-cbrack }
* l-sbrack [ 
* r-sbrack ]

#Grammar

###BNF
stmts   : stmt  
        | stmt stmts  
        ;  

stmt    : expr semicolon  
        ;  

expr    : expr + expr  
        | expr - expr  
        | expr * expr  
        | expr / expr  
        ;  

literals: float-lit  
        | double-lit  
        | integer-lit  
        | boolean-lit  
        | string-lit  
        | char-lit  
        | null-lit  
        ;

###Precedence rules:

left : + -  
left : * /  
