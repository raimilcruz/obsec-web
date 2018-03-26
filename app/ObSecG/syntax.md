#Generic ObSec.

##Syntax
```
e    ::=    x                                           (variable) 
            | e.m<I*>(e)                                 (method invocation)
            | new {z:S => mdef*}                     (object)
            
mdef ::=    m(x*)= e                                     (method definition)

T   ::=     X                                          (generic type variable) 
            | a                                         (self type variable)
            | Obj(a)[msig*]                          (object type)
            
l   :: =    T
            | X?
            | this
            | this?
            
I   :: =    T | l | *            
            
S    ::=    T<l                                         (security type)
msig ::=    m<x<:T>: S->S                               (method signature)
```


1. We added standard type variables to express parametric polymorphism. 
When type variable are used in the public type we can abstract over the policy of 
concrete objects. This is useful when implementing collections that does not access
to its elements.

    We also add an special type variable X?. X? has the same meaning of standard type variable,
    but in addition we can use it with different safety type in the same context. That is
    is valid to have String<X? and Int<X? in the same scope. This case the meaning is:

    * One type that is super type of Int and String
    * or,  the two safety types, ie String and Int respectively.

    Then the meaning of X? is contextual, it depends on the safety type is attached.

2. The other special constructor is the self type `this` . In many situation you want to 
say "the policy of this type is the same as mime". The is the case of the `trim` method
of `String` :

    ```
    String{
        ...
        String<this trim();    
        ...
    }
    ...
    String<StringLength s;
    s.trim() // have policy StringLength
    ```

    There is a limitation of this feature when the safety type is not the same that the defining type.
    For instance with the `hash` method. 
    
3. For this case we use an special version of the self label

    ```
    String{
        ...
        Int<this? hash();    
        ...
    }
    ```

    The label `this?` has the following behavior if the self label is instantiate to `String` (the public label
    of `String`) then `this?` will be the public label of Int (ie. Int). In other case it will be Top.


Let us dicuss now about how to use libraries that we programmed using those constructors.

1. All type variable in GObSec has a subtyping constraint, because of the well-formed condition of 
security types. In the case of variable of X?, the constraint look like X? < String ? Int
2. The self label is instantiated implicitly, that is once we create a security type 
(eg. String<StringLength)
3. The bottom self labe is also instantiated implicity.