#Generic ObSec.

##Syntax
```
e ::= x | e.m<T>(e) | new {z:S => mdef ...}
mdef ::= m<X<:T>(x)= e

T ::= X | a | Obj(a)[msig ...]
msig ::= m<x<:T>. T->T
```