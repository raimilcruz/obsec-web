#Generic ObSec.

##Syntax
```
e    ::=    x                                           (variable) 
            | e.m<I>(e)                                 (method invocation)
            | new {z:S => mdef ...}                     (object)
            
mdef ::=    m(x)= e                                     (method definition)

T   ::=     X                                          (generic type variable) 
            | a                                         (self type variable)
            | Obj(a)[msig ...]                          (object type)
            
l   :: =    T
            | X*
            | this
            | this*
            
I   :: =    T | l | *            
            
S    ::=    T<l                                         (security type)
msig ::=    m<x<:T>: S->S                               (method signature)
```