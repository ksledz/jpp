The language (called Lambada, with .lama extension) contains:

* Integers with + - * / () arithmetics and comparison
* if clause 
* variables and naming them 
* Booleans with standard operators
* named or unnamed multi-argument (or one of course) lambdas (functions) with recursion, also higher levels and partial application
* runtime and parse error handling with  messages and closing the interpreter
* standard library (stdlib.lama) that has lists using Church encoding, with functions "head", "tail", "empty", "is_empty" and "cons"
* a handful of examples showcasing all features

(I implemented everything for 25 points without static typing)

How to use: after installing stack, just launch stack run [file location] to execute a file, for example stack run examples/max.lama.
All good[number].lama files should return True, "bad" tests return various errors.


Grammar concept loosely described but the way its described based on EBNF (lets assume (sth)* means sth few times and (sth)+ is sth at leasr once
Bool: "True" | "False"
Integer: "1" | "2" | "3" | etc
String: literals
Comment: starts with # or a block comment """ \n comment \n """
Operator: "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "&&" | "||"  (- is only binary operator, no unary -)
Name: small-caps String
CapName: String starting with a capitalized letter
Program: program is one Expr

Expr: 
	Bool | Integer | 
	"if" Expr "then" Expr "else" Expr |
	Name | CapName | 
	"\" (Name)+ "->" Expr | "let" Name "in" Expr  - let is recursive
	Expr Expr | # partial application
	Expr Oper Expr | "(" Expr ")" 
to implement:
NamedExpr: "def" Name "=" Expr
TypeDef: "type" CapName (Name)* "=" Expr
DataDef: "type" CapName (Name)* "=" (enter indent "|" CapName (Type)*)* 
Type: "Bool" | "Int" | Type -> Type | Cap
Lists: empty | cons Bool/Integer list | head list | tail list


