```
e ::= x | e.m(e) | o
o ::= {z => m(x)=e ...}
U ::= Obj | a |  
T ::= T | E X!.T | X
S ::= T<U | T<-U
``` 

Example
```
val e : T<-E X!.U = ...
e1.m(e2) ... //what it the type of this expression?
```