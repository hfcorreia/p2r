## Available Tokens

### Binary Operators
* + - * /

### Literals
* integer
* real
* string
* char


## Grammar



Expr    : integer
        | Expr + Expr
        | Expr - Expr
        | Expr * Expr
        | Expr / Expr
        ;



Precedence rules:

left : + -
left : * /
