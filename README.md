# py2hs

## Introduction
I find it an interesting practice to learn and reinforce a new programming language by comparing it with 
a familiar language, and adding the distinct features and concepts from the new language on top of my existing knowledge system.
This is a reference guide I created when I was learning Haskell.

This project is also inspired by [py2rs](https://github.com/rochacbruno/py2rs). 

### Getting started with Haskell
Official Haskell documentation provides plenty of tutorials and books to start with.

https://www.haskell.org/documentation/


### This is 

- a complementary resource that helps python programmers to learn Haskell
- a place to learn and reinforce Haskell and its functional programming concepts


### This is NOT

- a one-stop shop that teaches everything you needs to know about Haskell

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
Python interactive shell examples starts with `>>> `, and Haskell GHCi examples starts with `λ> `. Note that all 
Python codes are written in [Python 3](https://docs.python.org/3/).

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

Approach 1: explicitly specify all inputs and the output

```haskell
bigger :: Int -> Int -> Int
bigger x y = if x > y
             then x
             else y
```

Approach 2: set a type constraint `Int` for `a` 
```haskell
bigger :: (Int a) => a -> a -> a 
bigger x y = if x > y
             then x
             else y
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

*Append*

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

*Prepend*

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

*List comprehension with conditions*

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

*More complex list comprehension*

Python

```python
>>> [i * j for i in range(1, 5) for j in range(1, 5)]
[1, 2, 3, 4, 2, 4, 6, 8, 3, 6, 9, 12, 4, 8, 12, 16]

>>> [[a, b, c] for c in range(1, 11) for b in range(1, c+1) for a in range(1, b+1) if a**2 + b**2 == c**2]
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
