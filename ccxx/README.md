# ccxx

[[_TOC_]]

## Todo list
- Name the project. Candidates:
  - Strafe

## Introduction

**_ccxx_** is a multi paradigm programming language that much like **_c++_** aims to facilitate the creation of low cost abstractions.

In its current state, this repository is little more than a statement of intent but it is actively being developed.

### Abstract design goals

- Has to be a library writers dream language.
- Make it easier to inter-operate with any other language.
- Reduce boilerplate by a significant amount.
- Focus on providing enough flexibility to allow implementation of things such as a borrow-checked in library code, and make said library code feel like a first class citizen.

## Specification

## General

Every name is allowed to be defined only once. Unlike **C++**, forward declarations of function prototypes or any other concept are not needed. Mutually dependent constructs are allowed and the compiler has to deal with that (?how).

## Tokens

### Comments

Line comments `//...` or block comments `/* ... */`

### Identifiers

Identifiers follow the well established pattern `[_a-zA-Z][_a-zA-Z0-9]+`

### Numeric constants

| Constant type         | Example        |
| :-------------------- | :------------- |
| Decimal Constants     | `1234`         |
| Hexadecimal constants | `0xccfA`       |
| Octal constants       | `0o7651`       |
| Binary constants      | `0b0100100111` |
| Real constants        | `1.23`         |

### Number separators

The digit `'` can be used as a number separator. A few examples:

```C++
1'000'000
0xFF'FF'FF'FF
```

 **??** Maybe prefer commas as digit separators. Would have to follow commas by spaces where it is used to delimit tokens. 

### String literals

Follow the pattern convention as C or C++:

```C++
"Hello World!"
```

### Suffixes

Identifiers may be used as suffixes for literal values, just like in **C++** with the following differences:

- Suffixes are scoped, even those built into the language.
- Any identifier can be a user defined suffix, not just those starting with `_`.

Examples:

- `120m` could stand for 120 meters, given all required definitions are in scope.
- `12.5l` could stand for 12.5 liters, ditto as above but could also stand for `long`, as it does in **C++**

## Type system

**type**: To be defined

**archetype**: To be defined

### Builtin types

In order to effectively interoperate with any language, first and foremost needs to support the list of fundamental types found in **C**. These will have the same semantics as they have in **C** on a given target platform.

- `bool`
- `char`
- `short`
- `int`
- `long`
- `long long`
- `float`
- `double`

In addition to the above types, **ccxx** brings some new built in types that are specific to the language and cannot be used for interop:

- `<type>`
- `<class>`
- `<field>`
- `<method>`
- `<constructor>`
- `<destructor>`
- `<function>`
- `<context>`
- `<callsite>`
- `<definition>`

### `<class>`

Denotes a metatype. Has the following signature:

```C++
<class> := class {

    /// Returns a list of all of a type's fields
    fields := \this -> list <field>
    methods := \this -> list <method>
    constructors := \this -> list <constructor>
    destructor := \this -> <destructor>
    definitions := \this -> list <definition>
}
```

### `<context>`

denotes any location in the AST.

### `<callsite>`

is a special type that may only ever be used as the first argument of functions. It's really a context under the hood and it's always the context where a function was called. Passed implicitly when functions taking it are called.

## Definitions

Every definition, be it a variable, function, custom type, namespace aso follows the pattern:

`identifier := expression`

### Bindings

Every name definition is in fact a binding. It binds the expression on the right to the name on the left. Some expressions may be evaluated immediately but some of them might not. This is a means to support laziness during compilation.

### Functions

Functions in ccxx are pure compile time constructs. Most of them will not produce runtime functions at all. The compiler will aggressively evaluate functions at compile time. This phase is called reduction. Only the function calls which couldn't be evaluated at compile time make it past this phase. Of those, the compiler will freely choose which ones will become runtime functions and which ones will simply be inlined.

Functions are first class, meaning that they can be passed around and new functions may be derived from existing ones by means such as functional composition and others too. To facilitate this, functions in **ccxx** are curried.

The simplest function possible is the one presented below. It takes an argument of any type and returns the argument unchanged. There is no type information at all in this function signature which means it relies entirely on type inference.

```C++
foo := function(arg) {
    arg
}

// C++ equivalent, before C++20:
template <typename T>
T foo(T arg) {
    return arg;
}

// Or in C++20
auto func(auto arg) {
    return arg;
}
```

The `\` character is used to disambiguate this as a function definition as opposed to a function signature (IE type of a function). Without the backslash prefix, the compiler would interpet `->` expressions as type signatures. When the prefix is found, the compiler will treat arbitrary identifiers as argument names. Without the prefix, arbitrary identifiers will be taken to denote argument types.

Here's a type signature which also happens to be the signature of the identity function presented above. It stands to mean: a function that takes an argument of any type, called `a` in the context of this signature, which returns a value of the same type `a`.

The question mark tells the compiler that `a` is not a specific type but a generic argument.

```C++
?argType -> argType

// Written in C++
template <typename argType>
using functionType = argType(argType);
```

Function arguments don't necessarily have to use type inference. Their types can be specified too. This simple function takes an `int` argument and returns its square.

```C++
\(int arg) = arg * arg

// Written in C++:
auto square(int arg) {
    return arg * arg;
}
```

Similarly, arguments may be generic but we can capture their type. This function takes an `arg` of any type called `typeOfArg` and a second argument of the same type, and returns the product of it's two arguments whose type is inferred.

```C++
\(?typeOfArg arg, typeOfArg arg2) = arg * arg2
// In C++, this would be written as:
template <typename typeOfArg>
auto func(typeOfArg arg, typeOfArg arg2) {
    return arg * arg2;
}
```

All of the above functions never specified their return type, which means that the expression after the last `->` was their body, and their result would simply be the result of that expression. Specifying the type is possible too, and an `=` sign is needed to separate the return type expression from the function body:

```c++
\(float arg) -> float = arg / 2

// Written in C++
float f(float arg) {
    return arg / 2;
}
```

Parameters may have default values. The default value expressions are written after the argument type, preceded by an `=`:

```C++
\(arg1, arg2 = 1) = arg1 * arg2
// Written in C++
template <typename T, typename U>
auto f(T arg1, U arg2 = 1) {
    return arg1 * arg2;
}
```

Or, if the arguments also have types:

```C++
\(int a1, float a2 = 1.) -> float = a1 * a2
// In C++
float f(int a1, float a2 = 1.) {
    return a1 + a2;
}
```

While many functions can be entirely written as one short expression, in real life we will go above that limit plenty of types. In that case, the function body may be contained within `{}` and the `return` keyword may be used to exit the function body at any time.

```C++
\(a1, a2, a3) {
    return 2
}
```

Functions may also simply not take arguments. In that case, the void type is used as a placeholder for the argument. It can also be used as a return type, when functions don't return any values. Examples:

```C++
// A function that takes no arguments and always returns value 12
\() = 12

// May also be written as:
\(void) -> int = 12

// A function that takes an argument x and prints it. Assuming print returns void, this function also returns void
\x -> print x

// May also be written as:
\x -> void = print x

// or
\x -> void {
    print x
}
```

All the above functions are simply expressions that can be used as anonymous functions. To really be able to call a function, it needs a name:

```C++
sum:= \(a, b) = a + b
```

### Function calls

Function calls are made by typing out a functions name followed by it's argument expressions. There are no `()` or separator between the arguments.

```C++
-- Given a function definition
sum:= \(a, b) = a + b
a = sum 2 3 // a = 5
```

#### Partial application

Functions calls may also be partial, in that a function that normally takes 2 arguments can be called with 1 argument. In that case, the result of the function call will be another function. To disambiguate partial calls within expressions, they must be surrounded by `()`. Example:

```C++
f1 := \(a, b) = a + b
f2 := \a = \b = f1 a b
f3 := f2 5
res := f3 4 // Will evaluate to 9
```

#### Passing arguments out of order (keyword arguments)

```C++
sub = \(int a, int b) -> int = a - b
result = sub b:2 a:4 // result will be 4-2=2
```

This can work well in conjunction with default parameters:

```C++
multiply = \(a, b = 2, c = 3) = a * b * c
result = multiply 2 c:4 // result will be 2 * 2 * 4 = 16
```

### Enums

```C++
E := enum {
    enumerator := expression,
    ...
}
```

### Structs and Unions

```C++
name := struct {
    // field: <type-yielding-expression>
    x := int
    y := int
}
```

### Classes

```haskell
name: class {

}

```

## Roadmap

- A bootstrap compiler is implemented in **_C++_**. The grammar is largely settled and core features tried and pruned. It can compile to **_llvm-ir_** and produce functional binaries. Parts of the language standard library are written too. They are used to test the compiler and the language.
- A compiler is implemented in **_ccxx_**. The list of language features is going to be settled at this stage.
- A package manager is implemented as library code and a central package repository is deployed. Now the language is presentable and can be tested by third parties.
