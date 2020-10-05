# Lambda Calculus Interpreter

An interpreter for [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus#:~:text=Lambda%20calculus%20(also%20written%20as,to%20simulate%20any%20Turing%20machine.)
, implementing
[De-Bruijn indexing](https://en.wikipedia.org/wiki/De_Bruijn_index).
It is written in [Haskell](https://www.haskell.org/).
with the parser defined using
[Happy](https://www.haskell.org/happy/).
This is a learning project,
so the [author](arkasparko@gmail.com)
will be really grateful
for bug reports and suggestions.

## Running
The Repl can be run by typing
```./repl ```
in linux terminal. 

## Syntax

The following are some examples of code
to help understand the syntax which
the interpreter accepts.
Lines starting with ``` >> ``` are
printed on the console
by the interpreter.

### Defining

``` let var = expression ```

defines ```var``` to be ```expression```.
#### Examples

``` let id = /x.x ```

``` let app = /f./x.(f x) ```

``` let id2 = ((app id) id) ```

### Printing
``` print expression ``` prints the ```expression```

#### Examples

```
print (id /y./x.x)
>> (/x0.x0 /x0./x1.x1)
```

Notice the renaming of bound variables.

### Evaluating
``` eval n expression ``` evaluates
(applies beta-conversion)
the ```expression``` ```n``` times.
We need to specify the number of evaluations, since evaluation of lambda expressions
can be non-terminating

#### Example

```
eval 2 ((app id) id) 
>> (/x0.x0 /x0./x1.x1)
```
### Evaluating and updating

``` evalDef n var```
evaluates the term definition of ```var```
```n``` times and updates the definition.

#### Example

``` evalDef 3 id```
Evaluates ``` id2 ``` three times and
updates the definition. So ```print id2```
outputs

```>> /x0.x0```

Notice that ```eval``` does not update the definition. So we have the following
```
---------------------
eval 3 id2
>> /x0.x0
---------------------
print id2
>> ((/x0./x1.(x0 x1) /x0.x0) /x0.x0)
---------------------
```


### Loading a file 
Type ```:l``` and press <kbd>enter</kbd>
.The interpreter will ask

```>> File to load?```

Write the file name and again
press <kbd>enter</kbd>
Inside a file we can write the same
syntax as repl(except loading a file),
in lines seperated by ```;```.

#### Example code inside file
```
let id = /x.x    ;
let K = /x./y.x ;
print id;
eval 2 (KComb id);
``` 

### Quitting
Type ```:q``` and press <kbd>enter</kbd>.
