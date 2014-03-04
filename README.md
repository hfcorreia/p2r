#Processing 2 Racket
Implementation of the Processing Language in DrRacket.

---
Reflects the current state that the parser supports!
#Available Tokens

##Binary Operators
* + - * /

##Literals
* integer
* float
* char


#Grammar

###BNF
Expr    : integer
        | Expr + Expr
        | Expr - Expr
        | Expr * Expr
        | Expr / Expr
        ;



###Precedence rules:

left : + -
left : * /
