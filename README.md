# py2hs

## Introduction
I find it an interesting practice to learn and reinforce a new programming language by comparing it with 
a familiar language, and adding the distinct features and concepts from the new language on top of my existing knowledge system.
This is a reference guide I created when I was learning Haskell.

This project is inspired by [py2rs](https://github.com/rochacbruno/py2rs). 

### Getting started with Haskell
Official Haskell documentation provides plenty of tutorials and books to start with.

https://www.haskell.org/documentation/


### This is 

- a complementary resource that helps python programmers to learn Haskell as a new language
- a place for beginners to learn and reinforce Haskell, as well as its functional programming concepts

### This is __not__

- an one-stop shop that teaches you *everything* about Haskell
- an encyclopedia that covers *all* the similarities and differences between Python and Haskell

## From Python to Haskell

##  General

| Definition      | Python      | Haskell  |
| -----------    | ----------- | -------- |
| [Programming paradigm](https://en.wikipedia.org/wiki/Programming_paradigm)   | [Imperative](https://en.wikipedia.org/wiki/Imperative_programming)  | [Purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming) |
| [Type System](https://en.wikipedia.org/wiki/Type_system)      | Dynamically typed | Statically typed|
| First appeared | 1989 | 1990 |
| File extensions | .py, .pyw, .pyc | .hs, .lhs | 
| Programming guidelines | [Haskell programming guidelines](https://wiki.haskell.org/Programming_guidelines#Let_or_where_expressions) | [PEP8](https://www.python.org/dev/peps/pep-0008/)


## Code

All examples are either code snippet or interactions with python shell or [Haskell GHCi](https://wiki.haskell.org/GHC/GHCi). 
Python shell examples starts with `>>> `, and Haskell GHCi examples starts with `λ> `. Please note that all 
Python codes are written in [Python 3](https://docs.python.org/3/).

For similarities, examples usually starts with Python, followed by Haskell. 
For concepts or features that are unique to Haskell, Haskell code will be shown first, 
and a Python implementation will follow. 

### Hello World

Python

```python
>>> print("Hello world")
Hello world
```

Haskell

```haskell
λ> print "Hello world"
"Hello world"
```

### Functions

#### Basic function definition
Python

```python
>>> def multiply(x, y):
...     return x * y
...
>>> multiply(2, 3)
6
```

Haskell

```haskell
λ> multiply x y = x * y
λ> multiply 2 3
6
```

#### Flow control
Python

```python
def bigger(x, y):
    if x > y:
        return x
    else:
        return y
```

Haskell

```haskell
bigger x y = if x > y
             then x
             else y
```

#### Explicit typing
Python

```python
def bigger(x: int, y: int) -> int:
    if x > y:
        return x
    else:
        return y
```

Haskell

##### Approach 1: explicitly specify all inputs and the output

```haskell
bigger :: Int -> Int -> Int
bigger x y = if x > y
             then x
             else y
```

##### Approach 2: set a type constraint `Int` for `a` 
```haskell
bigger :: (Int a) => a -> a -> a 
bigger x y = if x > y
             then x
             else y
```

#### Lambda function
Python

```python
>>> list(map(lambda x: x + 1, [1, 2, 3]))
[2, 3, 4]
```

Haskell

```haskell
λ> map (\x -> x + 1) [1, 2, 3]
[2,3,4]
```


### List


#### List creation

Python

```python
>>> [1, 2, 3, 4]
[1, 2, 3, 4]
>>> list(range(1, 5)) # Use range
[1, 2, 3, 4]
```

Haskell

Syntax sugar:

```haskell
λ> [1, 2, 3, 4]
[1,2,3,4]
λ> [1..4] -- Use range
[1,2,3,4]
```

Without syntax sugar:

```haskell
λ> 1:2:3:4:[]
[1,2,3,4]
```

#### List concatenation

Python

```python
>>> [1, 2] + [3, 4]
[1, 2, 3, 4]
```

Haskell
```haskell
λ> [1, 2] ++ [3, 4]
[1,2,3,4]
``` 

#### Adding element to list

##### Append

Python

```python
>>> my_list = [1, 2, 3, 4]
>>> my_list.append(5)
>>> my_list
[1, 2, 3, 4, 5]
```

Haskell
```haskell
λ> [1, 2, 3, 4] ++ [5]
[1,2,3,4,5]
``` 

##### Prepend

Python

```python
>>> my_list = [1, 2, 3, 4]
>>> my_list.insert(0, 5)
>>> my_list
[5, 1, 2, 3, 4]
```

Haskell
```haskell
λ> 5:[1, 2, 3, 4]
[5,1,2,3,4]
``` 

#### Indexing

Python

```python
>>> my_list = [1, 2, 3, 4]
>>> my_list[2]
3
```

Haskell
```haskell
λ> let myList = [1, 2, 3, 4]
λ> myList !! 2
3
```

#### List comprehension

Python

```python
>>> [i * 2 for i in range(1, 11)]
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
```

Haskell

```haskell
λ> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
```

##### List comprehension with conditions

Python

```python
>>> [i * 2 for i in range(1, 11) if i % 2 == 0]
[4, 8, 12, 16, 20]
```

Haskell

```haskell
λ> [x*2 | x <- [1..10], even x]
[4,8,12,16,20]
```

##### More complex list comprehension*

Python

```python
>>> [i * j for i in range(1, 5) for j in range(1, 5)]
[1, 2, 3, 4, 2, 4, 6, 8, 3, 6, 9, 12, 4, 8, 12, 16]

>>> [[a, b, c] for c in range(1, 11)
...     for b in range(1, c+1)
...     for a in range(1, b+1) if a**2 + b**2 == c**2]
[[3, 4, 5], [6, 8, 10]]

```

Haskell

```haskell
λ> [ x*y | x <- [1..4], y <- [1..4]]
[1,2,3,4,2,4,6,8,3,6,9,12,4,8,12,16]

λ> [ [a,b,c] | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
[[3,4,5],[6,8,10]]
```

#### Zipping

Python

```python
>>> list(zip(range(1,5), range(2,6)))
[(1, 2), (2, 3), (3, 4), (4, 5)]

>>> list(zip(range(1, 5), [8, 9, 10]))
[(1, 8), (2, 9), (3, 10)]
```

Haskell

```haskell
λ> zip [1..4] [2..5]
[(1,2),(2,3),(3,4),(4,5)]

λ> zip [1..4] [8,9,10]
[(1,8),(2,9),(3,10)]
```

### From Class (Python) to data (Haskell) 

In Python, and most OOP languages, Classes provide a means that bundles two things:
- data
- functions that retrieve, modify, or manipulate the data

Let's start with defining a class in Python.

```python
from dataclasses import dataclass

@dataclass
class Square:
    side: float

    def area(self):
        return self.side ** 2
```

```python
>>> my_square = Square(5)
>>> my_square.area()
25
>>> my_square.side = 6
>>> my_square.area()
36
```

Class `Square` bundles a float number, `side`, and a function, `area`, which calculates the area of the square. 
The attribute `side` could be directly updated. We use word "mutable" to describe the property that the attributes of 
a class could be changed. Because of mutability, the functions bundled with the class are stateful, which means that 
the outputs of the same function could be different depending on the state of the class. Being stateful is like holding 
a double-edge sword. On one hand, you can make the a class very flexible and adaptive to changes. On the other 
hand, however, the behavior of functions becomes non-deterministic with respect to their inputs, making it sometimes 
unpredictable and difficult to debug. 

Now let's see a way to define a Square and a function that calculates its area in Haskell.

```haskell
data Square = Square Float -- Defining "Square" as a data structure that holds a Float

area :: Square -> Float -- Declaring function "area" as a function that takes a "Square" and returns a Float
area (Square side) = side ^ 2 -- Implement the area function
```

```haskell
λ> let mySquare = Square 5
λ> area mySquare
25.0
```

What we saw above, is a data structure (`Square`) that holds a float, and a function (`area`) that operates on this specific
data structure, while not being bundled to the data structure itself. The separation of data and function, is one of the 
most important characteristics of Haskell. 
What's good about the separation of data and function? You are right, stateless! Instead of depending on some "internal"
state, functions are deterministic and predictable purely from their inputs.


### Interface and Typeclass

We know that in Python, a class can implement interface `__eq__(self, other)`, so its objects could be compared with 
the syntax `x == y`. Example:

```python
class MyClass:
    def __init__(self, val):
        self.val = val

    def __eq__(self, other):
        if isinstance(other, MyClass) and self.val == other.val:
            return True
        else:
            return False
```

```python
>>> MyClass(1) == MyClass(1)
True
>>> MyClass(1) == MyClass(2)
False
>>> MyClass(1) == 1
False
```

Typeclass in Haskell is similar to the concept of interface in object-oriented programming, 
but in a functional (better) way. A typeclass defines what 

Here is the definition of 
