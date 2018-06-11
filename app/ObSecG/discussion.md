Hola Éric,

Usemos este correo para establecer algunos conceptos para futuras discussion

Le he dado mil vueltas a lo último que hablamos y no logro tener nada concluyente sobre
como ser parámetrico y flexible en la definición de tipos.

Lo único que creo tener claro es que los builtin type tiene que ser tratados de forma
diferente a los user-defined types, lo cual repercurte en la formación de los security types
y la relación entre ambas facetas.

A continuación un dump de alguna ideas y desarrollos.

# Compartiendo ideas

La forma de especificar la interfaz "base" de un tipo difiere si el tipo es bultin 
(no implementando dentro del sistema, sino provisto a lo Abstract Data Type) o es un
user-defined type.

## User-defined types

La interfaz "base" debe permitir proveer una "implementación esperada" para el tipo.
Par ver esto, analizemos el siguiente tipo `PasswordChecker`:
```
{
    String<String login(String<H password, String<L guess);
}
```
El tipo `PasswordChecker` nunca podrá ser la faceta privada de un security type, pues
es imposible implementar (de la forma esperada) el método `login` recibiendo un 
parámetro completamente privado. 

Este ejemplo muestra que el programdor debe ser consciente que para dar una implementación
real, deberá especificar correctamente los tipos, requieriendo las operaciones necesarias
sobre los parámetros de entrada. 

En ese sentido, la implementación "gobierna"/"antecede" al tipo "base" que será usado para
la faceta privada. La siguiente implementación es explicíta en eso
```
{
  z: PasswordChecker<PasswordChecker
  =>
  def login(password,guess) = password.eq(guess)
}
```
Para que el método `login` sea bien tipado, es necesario que `PasswordChecker` sea
como mínimo definido como sigue:
```
{
  String<String login(String<StringEq password, String<String guess);
}
```
Esto está reflejado en el paper en la regla (TObj).

Resumen: La propia implementación pone la cota inferior de seguridad.

## Builtin types (ADT such as Int, String, Bool, ...)

En caso de estos tipos, su implementación no es provista dentro del lenguaje (por tanto
se salta las reglas del lenguaje). Por eso, es extremadamente importante proveer
interfaces que sean seguras (respecto a TRNI en este caso).

Para ilustrar esta necesidad supongamos que tenemos el tipo builtin `String` 
con la siguiente firma para su método `eq`:
```
{
   Bool<Bool eq(String<H other)
}
```
Esta firma no es conveniente desde el punto de vista de seguridad: podría
ser explotada de la siguiente forma:
```
String<H password = ...
String<L guess = ...
print(guess.eq(password)); //se asume print: Bool<Bool -> Unit<Unit
```

**Lo anterior muestra que la firma de cada método de un tipo builtin tiene que satisfacer
una conjunto de constraints que hagan su ejecución segura.** Tipicamente estas constraints
aparecen en los papers, como typing rules para los bultin types y builtin operators.

### Constrainst e instanciación

Veamos el tema de las constraints con el método `concat` de `String`. Comencemos por su
firma en Java:
```
String concat(String s);
```

luego en Jif tiene firma:
```
String@{this join labelof(s)} concat(String s); 
```

que responde a la idea de: "el resultado no expone más que los argumentos", que exactamente
lo que escribe en una typing rule para operadores binarios.

En ObSec y mediante uso de algo mecanismo de constraint, pudieramos escribar algo como:

```
String<r concat(String<l other) where l >: this join l
```

Los labels `r` y `l` que son posicionales (se instancian con el respectivo label
en su posición en la faceta públic) y `this` con la faceta pública

Veamos algunas interfaces que exponen el método `concat` y analizemos este constraint.

1. `concat` con misma política en parámetro y retorno:
    ```
    StringConcat{
        String<StringConcat concat(String<StringConcat a):
    }
    ```
    En este caso cuando se crea el security type `String<StringConcat` tenemos:
    
    * `this` |-> `StringConcat`
    * `r` |-> `StringConcat`
    * `l` |-> `StringConcat`
    
    lo cual **satisface** `r >: this join l`
    
2. `concat` que recibe `String` público y retorno `String` privado. 
    ```
        StringConcat2{
            String<String concat(String<String a):
        }
    ```
    La instanciación:
        * `this` |-> `StringConcat2`
        * `r` |-> `String`
        * `l` |-> `String`
    
    lo cual **no satisface** `r >: this join l`
    
    ***
    > En este punto es bueno hacer un paréntesis y hablar de que realmente significa
     algo como `String<String` dado que `String` es un **bultin type**.
    > 
    > Aquí hay dos conceptos: la definición de `String` con su conjunto de constraints 
     (faceta privada, abierta) y el uso de `String` como label de seguridad 
     (faceta pública; cerrada)
    > 
    > Empecemos por desambiguar la creación de un security type sobre un builtin type
     con otra syntaxis, digamos `T@l`.
    >
    > Lo otro, que significa ser el "label para String público" (notacion `StringL`)? Una idea para mi 
     es que tenga todos los métodos de la definición de `String`, donde todo label 
     paramétrico sea público también `StringL`. 
    > 
    > Entonces el `String@StringL` representaría el "String público".
    
    ***
    
    Volviendo al problema de `StringConcat2` y la no satisfacibilidad de la constraints, este 
    sugiere que las constraints pudiesen ser más permisivas.
    
    En ObSec verificamos "que el método esta en el tipo público". Si está, entonces es válido 
    usarlo. Entonces se puede usar esa idea, para que las contraints demanden cierta "simetría".
    o sea, si no puedo invocar `s1.concat(s2)` tampoco debería podría invocar `s2.concat(s1)`.
    
    Veamos otra propuesta de constraints para `concat` que capture lo anterior:
    ```
      String@r concat(String@l other) 
        where l <: {
                      String@{} concat(String@StringL a);
                   }
    ```
    
    Esta firma además dice que si puedo hacer `s1.concat(s2)` no necesito algo completamente
    público para el parámetro `other`, solo que tenga el método `concat`. Entonces lo más flexible
    que puedo ser es requerir una interfaz que retorne un `String` privado y demande 
    un `String` público
    
    Revisanto la interfaz `StringConcat` con las nuevas constraints tenemos:
    
    * `r` -> `StringConcat`
    * `l` -> `StringConcat`
                
    Se debe satisfacer entonces:
    ```
    {
        String@StringConcat concat(String@StringConcat a);
    }
    <:
    {
       String@{} concat(String@StringL a);
    }
    ```
    
    lo cual **se satisface**.       
    
    El punto más relevante que creo haber sacado, es que los bultin types, si los queremos
    flexibles (que puedan ser usados con distintos labels), tienen que tener otro tratamiento
    dentro de los security types.
    
    Un security type `T@l` no tendría que estar atado por subtyping, sino por alguna otra noción
    de abstraer. Podemos ver a `T` como una forma de proveer la interfaz safe del tipo y un conjunto
    de constraint que deben cumplir los métodos que quieran estar en la faceta pública. 
    
    Como materializar esto? Aun no lo tengo claro.
    
    Los dos últimos párrafos casi que tienen relación con la propuesta de extender ObSec con existencial
    types y la idea inicial que la faceta pública tenia que ser igual a la faceta privada modulo 
    substitución del tipo abstracto por sus tipo concreto.
    
### Resumen
   
En este este camino de tratar distinto los tipos bultin y 
los user-defined aparecer varias preguntas.
    
* En el tipo `T@l` donde `T` contiene un conjunto de métodos con sus constraints, como
se resuelven/instancian esas constraint ante la ausencia de esos métodos en la faceta pública.
    
    Por ejemplo:
    ```
    String@{} secret = ...
    secret.concat(...)...
    ```
    Cual sería la firma concreta de `concat` a usar cuando este no esta en la faceta pública?   

 * Se justifica crear una asimetría entre los tipos builtin y los user-defined types? En mi opinión
 parece necesaria dado que los bultin types no son implementandos usando las reglas del lenguaje.  
 * Sigue abierta la pregunta de como crear firmas genéricas para los métodos que interactuan
 con distintos tipos. Ejemplo: `bool eq(String other)` o `String concat(Int s)` .
 

Saludos,
R.     
    
    

