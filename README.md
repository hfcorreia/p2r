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
* char

##Seperators
* semicolon

#Grammar

###BNF
stmts   : stmt  
        | stmt stmts  
        ;  

stmt    : expr semicolon  
        ;  

expr    : integer  
        | expr + expr  
        | expr - expr  
        | expr * expr  
        | expr / expr  
        ;  

###Precedence rules:

left : + -  
left : * /  
