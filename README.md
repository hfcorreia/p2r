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

##Seperators
* semicolon

#Grammar

###BNF
stmts   : stmt  
        | stmt stmts  
        ;  

stmt    : expr semicolon  
        ;  

expr    : string  
        | integer  
        | float
        | double
        | boolean
        | char
        | expr + expr  
        | expr - expr  
        | expr * expr  
        | expr / expr  
        ;  

###Precedence rules:

left : + -  
left : * /  
