#Generic ObSec.

##Syntax
```
e    ::=    x                                           (variable) 
            | e.m<I*>(e)                                (method invocation)
            | new {z:S => mdef*}                        (object)
            
mdef ::=    m(x*)= e                                    (method definition)

T   ::=     X                                           (generic type variable) 
            | a                                         (self type variable)
            | Obj(a)[msig*]                             (object type)
            | T join T                                  (join/union type)
            
lc   :: =    X
            | a 
            | Obj(a)[msigc*]
            | XL                                        (low/type variable)
            
l  ::  =   lc            
            | this                                      (self label)                                              
            | thisL                                     (low/self label)
            
I   :: =    lc | *            
            
S    ::=    T<l                                         (security type)
msig ::=    m<varc>: S->S                               (method signature)

Sc   ::=    T<lc
msigc ::=    m<varc>: Sc->Sct                               (method signature)
varc ::=    X : T..T                                      (subtyping constraint)
            | XL : T;T                                 (low/subtyping constraint)
op   ::=    <:
            | >:
```


1. We added standard type variables to express parametric polymorphism. 
When type variable are used in the public type we can abstract over the policy of 
concrete objects. This is useful when implementing collections that does not access
to its elements.

    We also add an special type variable `XL`. 
    `XL` has the same meaning of standard type variable,
    but in addition we can use it with different safety type in the same context. 
    That is is valid to have `String<XL` and `Int<XL` in the same scope. 
    This case the meaning is:

    * One type that is super type of `Int` and `String`
    * or,  the two safety types, ie String and Int respectively.

    Then the meaning of `XL` is contextual, it depends on the 
    safety type is attached.

2. The other special constructor is the self type `this`. 
In many situation you want to say "the policy of this type is the same as mime". 
The is the case of the `trim` method of `String` :

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

    There is a limitation of this feature when the safety type is not the same 
    that the defining type. For instance with the `hash` method. 
    
3. For this case we use an special version of the self label

    ```
    String{
        ...
        Int<this? hash();    
        ...
    }
    ```

    The label `this?` has the following behavior if the self label is 
    instantiate to `String` (the public label of `String`) then `this?` will be 
    the public label of `Int` (ie. `Int`). In other case it will be `Top`.


Let us discuss now about how to use libraries that we programmed using 
those constructors.

1. All type variable in GObSec has a subtyping constraint, because of the 
well-formed condition of security types. In the case of variable of `XL`, the 
constraint use the constructor `;`: `XL < String ; Int`
2. The self label is instantiated implicitly, that is once we create a 
security type (eg. `String<StringLength`)
3. The bottom self label is also instantiated implicitly.



## Remarks
* Now, to verify for well-formed of security type When we create a type `T1<l`, 
we first need to instantiate the self labels (`this` and `thisL`) using the
the label `l`. Then we check for subtyping between the two facets.

It maybe means that we are not allowed to use the self labels in `l`. 
## Examples

```
typedef User = {
   self =>
   User@this couple();
   String@L name{};
}
typedef XUser = {
   self =>
   //hmmm problems here!
   XUser@this couple();
   String@L name();
   String@L age();
}
//other way

typedef User<User<this> :> this :> User<this>> = {
    self =>
    User@this
}
```


## Interfaces for Bool, Int, String and List (To Éric)
Slogan: "I want to be polymorphic respect to label until you create a security type"

```
Bool = {

    //-> if
    //The standard signature is:
    //T if[T]({T apply();} t, {T apply();} f);
    
    //In Jif is could be like this.
    //T@{this join l1 join l2} if[T2][label l1][label l2]({T@l1 apply();} t, {T@l2 apply();} f);
    
    //In ObSec, for the case of integer, would be this
    //Int<Int if({Int<Int apply();} t, {Int<Int apply();} f);
    //
    //However, it not possible to use it with branches different that LOW.
    
    //In ObSec, with generics (but without generic labels)
    //T<T if[T]({T<T apply();} t, {T<T apply();} f);
    //
    //However, it has the same problem that above.
    
    //We want to capture that the result depends on:
    //1. The current instances of bool if is LOW or HIGH
    //2. Both branches
    //
    //If the current boolean is HIGH, then the result is HIGH. We can not 
    //control this fact (at the definition time).    
    //
    //So we only control when the "if" method is in the declassification type
    //In that context, we can make the signature of "if" more rigid or flexibile
    //depending of the security concerns.
    //To satisfy constraint 2 we can implement this.
    
    //if the boolean is LOW, then the result is T join l
    //T<{r} if[T, l1 >: T1, l2 >: T2, r >: l1 join l2]({T<l1 apply();} t, {T<l2 apply();} f);
    //
    //However the above signature is too explicit, we can use a more "implicit" signature if
    //we have TSub in the language:
      
    T<{l} if[l,T<:l]({T<l apply();} t, {T<l apply();} f);
    
    //<- if
}
```

With the above definition of `Bool` we can encode the `if` control flow instruction.
Example:

```
String<H password = ...
String<L guess = ....
if(someBoolean)
  password
else
  guess  
```

to 

```
String<H password = ...
String<L guess = ....
someBoolean
    .if[H,String](
        {String<H apply()=> password},
        {String<L apply()=> guess}
    )
```

Let's see the interface for `String`

```

String = {
    //--> eq method:
    //In an non-security OO language the signature is what follows:
    //Bool ==(String s); 
    
    //For a IFC point of view what we want to capture is that the result must
    //be as secret as the self String and the arguments of eq:
    //In JIF the signature is what follows:
    //Bool@{this join l} eq[l][String@l other]
    
    //With faceted-types we know that the result of invoking eq will be
    //HIGH when the declassification policy (attached to String) does not 
    //containt eq. So we need just to care when the eq method is in the 
    //declassification policy. Then the signature that we put here on eq is just 
    //to constraint what the signature of the eq method in the policy is allowed 
    //to expose.
    
    //A monomorphic version of this method in ObSec could be:
    //bool<bool eq(String<String other);
    
    //With the above version a policy StringEq can only abstract the resulting
   //policy, but it can touch the argument. Example:
   //
   //StringEq1 = { bool<H eq(String<String other);} is a valid policy for String
   //
   //However, 
   //
   //StringEq2 = {bool<bool eq(String<H other);} is not a valid policy
   //because String<H is not subtype of String<String. 
   
   //The above aspect is not satisfactory in general, one could be more relaxed 
   //and yet to be sound.
   
   //For instance we can express that the argument must be as "public" as the
   //self label       
   //bool<bool eq[lm<:this](String<lm other);
   
   //Then StringEq1 is still a valid declassification type for String. 
   //Note that when we create the security type String<StringEq1, we close the private facet,
   //then the method eq has now signature (in the private facet):
   //bool<bool eq(String<StringEq1 other)
   //
   //Note that the eq methods are in the subtyping relation:
   //String<String <: String<StringEq1. However String<H as argument of eq in 
   //StringEq1 is not valid.
   //However, no StringEq3 = {bool<bool eq(String<StringEq a2}} is a valid 
   //declassification type for String... Nice!
   //Then we conclude that this signature is more appropiated for eq method in 
   //String:
   
   bool<bool eq[l<:this](String<l other);
   
   //Note that there is no problem with bool<bool as return method because the
   //declassificaiton policy always can refined that to anything between 
   //Bool and Top. Nice!!
   
   //--> TRIM
   
   //simple version:
   //String trim();
   
   //JIF version
   //String@this trim();
   
   //ObSec version
   //String<String trim();
   
   //GObSec version
   String@this trim();
   //<-- TRIM
   
   //--> Concat
   //simple version:
   //String concat(String s);
   
   //Jif version
   //String@{this join l} concat[l](String@l a;)
   
   //ObSec version
   //String@String concat(String@String a)
   
   //GObSec version   
   //String@l concat[l<:this](String@l a)   
   //<-- Concat   
}
```

Let's the interface for `Int

```
Int = {
    //simple version:
    //Int add(Int a);
    
    //JiF Version
    //Int@{this join l} add[l](Int@l a);
    
    //ObSec conservative version    
    //Int@Int add(Int@Int a);
    
    //ObSec parametric version
    Int@l add[l<:this] (Int@l a);
}

```
Let's the interface for `List`
```

List[l,X<:L]{
   Bool<Bool isEmpty();
   X<L head();
   List[L,X]<List[L,X] tail();
}
//another option to define List is to abstract over security types
List[S]{
   Bool<Bool isEmpty();
   S head();
   List[S]<List[S] tail():  
}
//then we can instantiate it with Int<Eq[Int]
//A great different between data-structures in our approach and 
//data-structure in standard/jif approaches is that label of the element
//does not affect the label of the data-structure (at the end they are different
//types)
//For instance we can implement a generic method length for list without
//to take care of the label of the elements:
//
Int<Int length[S](List[S]<List[S] l){
   if(l.isEmpty()) 0
   else length[S](l.tail).add(1) 
}
//then we can use the length method with a list of secret passwords
List[String<H] passwords = ...
Int<Int l = length[String<H](l)
```


## Final explanation

- From an implementor perspective we define a single type (not a faceted type)
 (for instance String, Int, PasswordChecker)
- From a client perspective, he consumes a security type. So it never uses 
 directly the facet, but after the creation of a security type. 
 The pairing of two types to form a security type has more
 meaning that we anticipate.
- To remark the above point: THERE IS NO WAY of using an object without a 
security type. A place to see that is the definition of an object (the object 
itself must know its security type). This is huge!!
- That's said. At the moment of defining a type we have a decision to take: to 
provide concrete declassification interfaces or to left open the specification
of declassification interfaces to the moment of creating a security type. In the
former case declassification types are taken from the attached declassification 
type.
- We need to name the "method-dependent label", because in many scenarios despite 
we do not know who is, we want to constraint it.

Let us explain how the self label works by examples. Recall the `String` and
`StringEq` interfaces:
```
String = {
    ...
    Bool<Bool eq[lm <: this](String@lm a): 
    ...
}
StringEq = {
    ...
    Bool<Bool eq(String@StringEq a): 
    ...
}
```

The label `lm` is a declassification-provided label, ie. it will be provided
when creating a security type.
For instance when we create `String<StringEq` the following things happens:
- The `this` parameter is instantiated, in this case to `StringEq`
- We extract the type to instantiate the label `lm`. In this case it corresponds
to label of the parameter `a` which is `StringEq`. 
-  Then we need to verify if the constraint over `lm` (`lm < this`) is 
satisfied by StringEq, which is the case `StringEq <: StringEq`
- Then we obtain the instantiated private facet, in this case is:
   ```
   String' = {
        ...
        Bool<Bool eq(String@StringEq a);
        ...  
   }
   ``` 
- Then we verify that `String' <:StringEq` which is case.
- After doing all these step we can assert the security type is well-formed.

In some case is not necessary to name the declassification-provided label because
we do not constraint it. In that case we can use a wildcard label `l*`

There are a few technical considerations with declassification-provided labels
- If the declassification type does not contains a method, what are the type
to instantiate those labels?. For instance what happen to labels of the method
`concat` in the security type String<StringEq. It is sound to assume the LOW
label everywhere? For the concat method it works, however, .... maybe a 
condition would be that the signature must work if change all 
declassification-provided labels by LOW.
Yes it sound to assume anything, since the result of using the method `concat`
or other method that is not in the declassification policy will be HIGH. Then
it will be protected by HIGH. In fact the rule should be:
- HIGH for all inputs
- LOW for all outputs.
Since que want to be flexible in this case regarding the HIGH-observers.
 
- I have to think about non-satisfiable constraints. 

So far we have presented the "self label" with an own syntactic constructor. 
However the self label is just an special case of declassification-provided 
label, but instead of be taken from it is position in a method signature in the 
declassification type, it is the whole declassification type. We can unify the 
syntax for the self label in the following way:

```
String<l1{
   ...
   String<l1 eq[lm <: l1](String<lm a);        
   ...   
}
```  

It is position a type level definition give the idea that is taken from the 
concrete declassification policy. In the formal model for the paper we can 
express this with the following notation:

```
Obj(z,l1)[
   ... 
   z<l1 eq[lm <: l1](z<lm a);
   ...    
]
```

       