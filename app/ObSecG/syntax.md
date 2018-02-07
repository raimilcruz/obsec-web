#Generic ObSec.

##Syntax
```
e    ::=    x                                           (variable) 
            | e.m<T>(e)                                 (method invocation)
            | new {z:S => mdef ...}                     (object)
            
mdef ::=    m(x)= e                                     (method definition)

T,U   ::=    X                                          (generic type variable) 
            | a                                         (self type variable)
            | Obj(a)[msig ...]                          (object type)
S    ::=    T<U                                         (security type)
msig ::=    m<x<:T>: S->S                               (method signature)
```