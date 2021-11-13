# py2hs

## Introduction
I find it an interesting practice to learn and reinforce a new programming language by comparing it with 
a familiar language, and adding the distinct features and concepts from the new language on top of my existing knowledge system.
This is a reference guide I created when I was learning Haskell.

This project is inspired by [py2rs](https://github.com/rochacbruno/py2rs). 

*This is:* 

- a complementary resource that helps python programmers to learn Haskell as a new language
- a place for beginners to learn and reinforce some of the core concepts in Haskell

*This is __not__:*

- an one-stop-shop that teaches *everything* about Haskell
- an encyclopedia that aims at covering *all* similarities and differences between Python and Haskell

## Table of contents
  * [General](#general)
     * [Getting started with Haskell](#getting-started-with-haskell)
  * [Functions](#functions)
     * [Basic function definition](#basic-function-definition)
     * [Flow control](#flow-control)
     * [Explicit typing](#explicit-typing)
     * [Lambda function](#lambda-function)
     * [Comments](#comments)
  * [List](#list)
     * [List creation](#list-creation)
     * [List concatenation](#list-concatenation)
     * [Adding element to list](#adding-element-to-list)
     * [Indexing](#indexing)
     * [List comprehension](#list-comprehension)
     * [Zipping](#zipping)
  * [Class -&gt; data](#class---data)
  * [Interface -&gt; Type class](#interface---type-class)
  * [Higher order functions](#higher-order-functions)
     * [Map](#map)
     * [Filter](#filter)
     * [Reduce](#reduce)
     * [Partial functions](#partial-functions)
 * [Functor](#functor)
 * [Applicative](#applicative)
 * [Monoid](#monoid)
 * [Monad](#monad)


## General

| Definition                                                                 | Python                                                             | Haskell                                                                           |
| -------------------------------------------------------------------------- | ------------------------------------------------------------------ | --------------------------------------------------------------------------------- |
| [Programming paradigm](https://en.wikipedia.org/wiki/Programming_paradigm) | [Imperative](https://en.wikipedia.org/wiki/Imperative_programming) | [Purely functional](https://en.wikipedia.org/wiki/Purely_functional_programming)  |
| [Type System](https://en.wikipedia.org/wiki/Type_system)                   | Dynamically typed                                                  | Statically typed                                                                  |
| First appeared                                                             | 1989                                                               | 1990                                                                              |
| File extensions                                                            | .py, .pyw, .pyc                                                    | .hs, .lhs                                                                         |
| Programming guidelines                                                     | [PEP8](https://www.python.org/dev/peps/pep-0008/)                  | [Haskell programming guidelines](https://wiki.haskell.org/Programming_guidelines) |

### Getting started with Haskell
Official Haskell documentation provides plenty of tutorials and books to start with.

https://www.haskell.org/documentation/

### Code format

All examples are either code snippet or interactions with python shell or [Haskell GHCi](https://wiki.haskell.org/GHC/GHCi). 
Python shell examples starts with `>>> `, and Haskell GHCi examples starts with `λ> `. Please note that all 
Python codes are written in [Python 3](https://docs.python.org/3/).

---------------------------

### Functions

#### Basic function syntax
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

#### Comments
Python

```python
# This is a python comment
'''
Python
multiline
comment
'''
```

Haskell

```haskell
-- This is a haskell comment
{-
haskell
multiline
comment
-}
```

---------------------------

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

##### More complex list comprehension

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

---------------------------

### Class -> data 

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
Attribute `side` could be directly updated. The functions bundled with the class are stateful, because 
the outputs of the same function could be different depending on the state of the class. Being stateful is like holding 
a double-edge sword. On one hand, you can make a class very flexible and adaptive to new changes. On the other 
hand, however, the behavior of functions could be non-deterministic with respect to their inputs, making them 
unpredictable, and therefore, difficult to debug. 

Now let's see a way to define a Square and a function that calculates any Square's area in Haskell.

`data` is a keyword in Haskell. It defines a type of data structure, similar to `Class` in Python.

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

What we saw above, is a type (`Square`) that holds a float, and a function (`area`) that operates on this specific
type, while not being bundled to the data structure itself. The separation of data and function, is one of the 
most important characteristics of Haskell. 
What's good about the separation of data and function? Stateless! Instead of depending on some "internal"
state, functions are deterministic and predictable purely from their inputs.

---------------------------

### Interface -> Type class


Area does not only apply to square, but all types of shapes. Let's start with defining an interface, `area`, in Python.

```python
from abc import ABC, abstractmethod

class Shape(ABC):

    @abstractmethod
    def area(self):
        pass
```

Then we define a couple of shapes that implements `area`.

```python
import math
from dataclasses import dataclass

@dataclass
class Square(Shape):
    side: float

    def area(self):
        return self.side ** 2

@dataclass
class Rectangle(Shape):
    height: float
    width: float

    def area(self):
        return self.height * self.width

@dataclass
class Circle(Shape):
    radius: float

    def area(self):
        return math.pi * self.radius ** 2
```

```python
>>> my_shapes = [Square(5), Rectangle(5, 6), Circle(3)]
>>> [shape.area() for shape in my_shapes]
[25, 30, 28.274333882308138]
```

How do to do so in Haskell?

In Haskell, data can define more than one constructor.

```haskell
data Shape = Square Float | Rectangle Float Float | Circle Float
```


Here is one way to write our area function in Haskell:

```haskell
area :: Shape -> Float
area (Square side) = side ^ 2
area (Rectangle height width) = height * width
area (Circle radius) = pi * radius ^ 2
```

```haskell
λ> map area [Square 5, Rectangle 5 6, Circle 3]
[25.0,30.0,28.274334]
```


You might already observe a problem here: whenever a new type of "Shape" is created, we will need to add that type to 
the implementation of "area".

For instance, if we create a new type that represents continents and we want them to support calculation of 
their areas. If we continue to use the same area function, things become ugly.

First, we need to add more types to shape:

```haskell
data Shape = Square Float
    | Rectangle Float Float
    | Circle Float
    | Africa
    | Antarctica
```

Then, we need to modify area function as well

```haskell
area :: Shape -> Float
area (Square side) = side ^ 2
area (Rectangle height width) = height * width
area (Circle radius) = pi * radius ^ 2
area (Africa) = 1.117e7 -- in the unit of square miles
area (Antarctica) = 5.483e6
```

In other words, the implementation of `area` is coupled with `Shape`. 


Here is where type classes become handy. Type class is similar to the concept of interface in 
object-oriented programming. Let's try to define our "interface" using a type class.


```haskell
class MyObject a where
    area :: a -> Float
```

What we are are seeing, is a type class, `MyObject`, that declares a behavior (or interface), `area`, 
which returns a Float when a type of `MyObject` is given. Notice that the `class` in Haskell is not the same as the 
"class" in Python. Instead, it is more similar to an abstract class in Python.

Now, we can define Shapes and Continents separately and make them support area calculation at the same time.

##### Define shapes 

```haskell
data Shape = Square Float | Rectangle Float Float
instance MyObject Shape where
    area (Square side) = side ^ 2
    area (Rectangle height width) = height * width

```

A new Haskell keyword, `instance`, is used here. `instance` declares that a data type is to implement all 
"interfaces" defined in a type class. In our case, `instance MyObject Shape` could be read as `MyObject` will implement 
all functions declared in type cass `Shape`. In Python, it will look something like this `class MyObject(Shape):`.

```haskell
λ> let a = Square 4
λ> area a
16.0
λ> let b = Rectangle 4 5
λ> area b
20.0
```

##### Define continents

```haskell
data Continent = Africa | Antarctica
instance MyObject Continent where
    area Africa = 1.117e7
    area Antarctica = 5.483e6
```

```haskell
λ> area Africa
1.117e7
λ> area Antarctica
5483000.0
```

The codes of Shape and Continent are completely independent, but we can still apply the same function (`area`) on them. 

This concept is actually nothing new in Python. 
Remember [data model](https://docs.python.org/3/reference/datamodel.html)? Yes, type class is a more power version of 
data model, which is a list of pre-defined interfaces by Python, while type class can define any possible interfaces.

In python, a class can implement  `__eq__` so we can apply `==` operator to it:


```python
class Rectangle:
    def __init__(self, height, width):
        self.height = height
        self.width = width

    def __eq__(self, other):
        if isinstance(other, Rectangle):
            return self.height == other.height and self.width == other.width
```

```python
>>> Rectangle(5, 6) == Rectangle(5, 6)
True
>>> Rectangle(5, 6) == Rectangle(6, 7)
False
```

Unsurprisingly, `==` belongs to type class `Eq` in Haskell.

This is the definition of `Eq`:

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)  
    x /= y = not (x == y)   
```

Let's try implement `Eq` for our rectangle.

```haskell
data Rectangle = Rectangle Float Float
instance Eq Rectangle where
    Rectangle a b == Rectangle x y = a == x && b == y
```

```haskell
λ> Rectangle 5 6 == Rectangle 5 6
True
λ> Rectangle 5 6 == Rectangle 6 7
False
λ> Rectangle 5 6 /= Rectangle 6 7
True
```

Notice that we only need to implement either `==` or `/=`, and the other operator will be automatically usable. 
This is achieved by the default implementation of `Eq`, where one is the negation of the other:

```haskell
    x == y = not (x /= y)
    x /= y = not (x == y)
``` 

---------------------------

### Higher order functions

Higher order functions are functions that takes functions as inputs or return functions as outputs.
Let's start with some of the most common higher order functions: map, filter, and reduce.

#### Map
Python

```python
>>> list(map(lambda x: x*2, range(1, 5)))
[2, 4, 6, 8]
```

Haskell

```haskell
λ> map (*2) [1..4]
[2,4,6,8]
```

#### Filter
Python
```python
>>> list(filter(lambda x: x >= 3, range(1, 5)))
[3, 4]
```


Haskell
```haskell
λ> filter (>=3) [1..4]
[3,4]
```

#### Reduce
Python
```python
>>> from functools import reduce
>>> reduce(lambda x, a: x + a, range(1,5), 100)
110
```

Reduce is called "fold" in Haskell
```haskell
λ> foldl (+) 100 [1..4]
110
```

#### Partial functions

In Python, a function can be passed as an argument to another function, which is called higher order function. 
For example, we can create a higher order function that supplies default arguments to other functions. Below, we 
have function `add` and `multiply` that both take two input arguments.

```python
def add(x, y):
    return x + y

def multiply(x, y):
    return x * y
```

Now, we can create a higher order function that applies default input argument to the two functions above. 
```python
def partial(func, *partial_args):
    def apply(*remaining_args):
        return func(*partial_args,  *remaining_args)
    return apply
```

Now, we can use a combination of `partial` and another function to construct interesting functions. 

```python
>>> increment = partial(add, 1)
>>> increment(2)
3
>>> increment(3)
4
>>> double = partial(multiply, 2)
>>> double(2)
4
>>> double(3)
6
```

In fact, Python standard library provides `partial` as a utility function in module `functools`.

```python
from functools import partial
>>> from functools import partial
>>> increment = partial(add, 1)
>>> increment(3)
4
>>> double = partial(multiply, 2)
>>> double(2)
4
```

Can we do something similar in Haskell? Yes, and even simpler! 

```haskell
λ> increment = (+) 1
λ> increment 3
4
λ> increment 4
5
λ> double = (*) 2
λ> double 2
4
λ> double 3
6
```

In fact, all functions could be partially applied in Haskell by default. In Haskell, we call partially 
applied functions as "curried functions", named after [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry).

---------------------------

### Functor

In Python, [`map`](https://docs.python.org/3/library/functions.html#map) is very useful when we want to transform 
an iterable.

For example, we can transform a list of integers to a list of strings with `map`.

```python
>>> list(map(str, [1, 2, 3, 4, 5]))
['1', '2', '3', '4', '5']
```

Here, list is a "container" that holds the elements, each of which is mapped to a new element.

A limitation of `map` is that the container has to be an iterable. 


Say we have a tree structure like below:

```python
class Tree:
    def __init__(self, left=None, right=None, val=0):
        self.left = left
        self.right = right
        self.val = val

    def __repr__(self):
        return f'Tree(val={self.val}, left={self.left}, right={self.right})'
```


```python
>>> my_tree = Tree(left=Tree(val=1), right=Tree(val=2))
>>> my_tree
Tree(val=0, left=Tree(val=1, left=None, right=None), right=Tree(val=2, left=None, right=None))

```

Clearly, `map` can't transform the values in the tree structure. In this case, it is easier to implement our own `map`.

```python
def map_tree(my_func, tree):
    if tree:
        return Tree(left=map_tree(my_func, tree.left), 
                    right=map_tree(my_func, tree.right),
                    val=my_func(tree.val))
``` 

```python
>>> map_tree(lambda x: x*2, my_tree)
Tree(val=0, left=Tree(val=2, left=None, right=None), right=Tree(val=4, left=None, right=None))

>>> map_tree(lambda x: 'transformed!', my_tree)
Tree(val=transformed!, left=Tree(val=transformed!, left=None, right=None), right=Tree(val=transformed!, left=None, right=None))
```

Now, we have a default `map` function from Python standard library, and we also have our own implementation of 
`map_tree`, implemented for `Tree` structure. 
Unfortunately, there is not a straightforward way to unify these two functions in Python. By 
unify, we mean a universal mapper that can apply a function to list, Tree, and essentially any type of data structures.

Thanks to the robustness of type class, this universal mapper is possible in Haskell, and it is provided as part of 
Haskell's standard library. It is called "Functor".

Here is the definition of functor:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

`f` is a type of container that "contains" data. It could be a list, a Tree, or anything that holds data. 
`(a -> b)` means a function that transform a value of type `a` to a value of type `b`. `f a` is a concrete type that 
means container `f` is holding values of type `a`. `f b` means container `f` holds values of type `b`.

All together, the definition of Functor in plain words, is something that implements a function `fmap`, that, 
given a function (`(a -> b)`) and a container `f` that holds values of type `a`, returns a container `f` that holds 
values of type `b`. `fmap` is the universal map function we've been looking for.

Let's look at the implementation of Functor for list.

```haskell
instance Functor [] where  
    fmap = map  
``` 
 
It is really straightforward. The Functor implementation of list is simply `map`. 

```haskell
λ> fmap (*2) [1,2,3]
[2,4,6]

``` 

Now, let's create a Tree type and see how Functor can work in such context. 

```haskell
λ> data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
λ> tree = Node 0 (Node 1 EmptyTree EmptyTree) (Node 2 EmptyTree EmptyTree)
λ> tree
Node 0 (Node 1 EmptyTree EmptyTree) (Node 2 EmptyTree EmptyTree)
```

Implementing `fmap` for `Tree`:

```haskell
instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)  
```

```haskell
λ> fmap (*2) tree
Node 0 (Node 2 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)
λ> fmap (\x -> "Transformed!") tree
Node "Transformed!" (Node "Transformed!" EmptyTree EmptyTree) (Node "Transformed!" EmptyTree EmptyTree)
```

We can see that `fmap` could be applied to any type of "container" as long as it is an instance of Functor.

---------------------------

### Applicative

Applicative is a more powerful type of Functor.

In the section of Functor, we learn how to apply a function to a Tree with `fmap`.

What if we have two trees and we want to overlay the two to generate a new Tree? 

```
  Tree 1          Tree 2              New Tree

    1       +       5       --->         6                
   / \             /                    /
  2   3           6                    8
```

What if we want to apply three trees to a function?
```
 
   Tree 1          Tree 2         Tree 2              New Tree
 _                        _
|    1               5     |        5                    30                
|   / \      +      /      |   *   /        --->        /
|  2   3           6       |      6                   48
 -                        - 
```

First, let's see how to implement this in python.

We can't use `map_tree` created previously, because it only operates on one tree. We can create a 
functions that can operates on any number of trees:

```python
def map_trees(trees, func):
    if all(trees):
        return Tree(val=func(*[tree.val for tree in trees]), 
                    left=map_trees([tree.left for tree in trees], func), 
                    right=map_trees([tree.right for tree in trees], func))
    else:
        return None
```

```python
>>> tree1 = Tree(val=1, left=Tree(val=2), right=Tree(val=3))
>>> tree2 = Tree(val=5, left=Tree(val=6))
>>> map_trees([tree1, tree2], add)
Tree(val=6, left=Tree(val=8, left=None, right=None), right=None)
>>> map_trees([tree1, tree2], pow)
Tree(val=1, left=Tree(val=64, left=None, right=None), right=None)
>>> map_trees([tree1, tree2, tree2], lambda a, b, c: (a + b) * c)
Tree(val=30, left=Tree(val=48, left=None, right=None), right=None)
```

Intuitively, `map_trees`, which accepts any numbers of trees as inputs, is a more powerful version of `map_tree`, which
takes only two trees as inputs. Similarly, applicative is a more powerful version of functor, in a way that 
it could be passed to a function that has more than one input. In Haskell, applicative is defined as below:

```haskell
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
```

First, the type constraint tells us that applicative has to be a Functor in the first place. Second, applicative has two 
functions. `pure` is basically wrapping a value in a functor (or "container"). `<*>` is similar to `fmap`. A 
quick reminder of `fmap`:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```
The only difference between `fmap` and `<*>` lies in their first argument, where functor is looking for a function, 
`(a -> b)`, while applicative is looking for a function that is wrapped in a functor, `f (a -> b)`.

Seems weird? Let's look how we can implement `map_trees` with applicative in Haskell.

```haskell
instance Applicative Tree where
    pure x = Node x (pure x) (pure x)
    Node a aLeft aRight <*> Node b bLeft bRight = Node (a b) (aLeft <*> bLeft) (aRight <*> bRight) 
    _ <*> _ = EmptyTree
```

What `pure` does is simply placing a value inside a container (a tree in the case above).
For `<*>`, the values of two Trees are combined by applying `b` to `a` recursively when both inputs are not empty.
For the rest of cases, simply return an `EmptyTree`. With `Tree` being an applicative, we can now apply a function takes
X number of inputs to X number of trees.

```haskell
λ> tree1 = Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)
λ> tree2 = Node 5 (Node 6 EmptyTree EmptyTree) EmptyTree
λ> pure (+) <*> tree1 <*> tree2
Node 6 (Node 8 EmptyTree EmptyTree) EmptyTree
λ> my_func a b c = (a + b) * c
λ> pure my_func <*> tree1 <*> tree2 <*> tree2
Node 30 (Node 48 EmptyTree EmptyTree) EmptyTree
λ> my_func a b c d = a * b + c * d
λ> pure my_func <*> tree1 <*> tree2 <*> tree2 <*> tree2
Node 30 (Node 48 EmptyTree EmptyTree) EmptyTree
```

In the code above, we can pass multiple trees as inputs to a pure function by connecting them with `<*>`,
e.g. `pure (+) <*> tree1 <*> tree2`. How and why does this work exactly? 

If we look at `pure (+)` carefully, it is an applicative that contains a function with type `a -> a -> a`.
```haskell
λ> :t pure (+)
pure (+) :: (Applicative f, Num a) => f (a -> a -> a)
```

Intuitively, `pure (+)` could be visualized as an abstract container whose element is `(+)`. When the container is 
`Tree`, for example, it could be visualized as an infinity Tree where each node contains `(+)` a function, as visualized 
below. One thing to be clear is that this "addition tree" is not fully constructed in memory. Instead, the nodes will be 
lazily created as we apply another Tree to it with `<*>`. 

```
Applicative Tree of (+)

         (+)
        /  \
      (+)  (+)
      / \  / \
    (+)(+)(+)(+)
   . . . . . . . .  
```

Now if we apply a Tree to `pure (+)` using `<*>`, we end up with a concrete  
Tree applicative that contains a function of type `a -> a`. The type wrapped in the applicative has now changed from 
`a -> a -> a` (addition function, `(+)`) to `a -> a`, because the first argument of the addition function has been filled 
with `tree1`.

```haskell
λ> tree1 =  Node 1 (Node 2 EmptyTree EmptyTree) (Node 3 EmptyTree EmptyTree)
λ> :t pure (+) <*> tree1
pure (+) <*> tree1 :: Num a => Tree (a -> a)
```

`pure (+) <*> tree1` could be visualized as below:

```
     (+)1
     /  \
    /    \
  (+)2   (+)3
```

With `pure (+) <*> tree1`, we can apply it with another Tree using `<*>` again, and finally we get a Tree that contains 
just numbers.
```haskell
λ> tree2 = Node 5 (Node 6 EmptyTree EmptyTree) EmptyTree
λ> result = pure (+) <*> tree1 <*> tree2
λ> result
Node 6 (Node 8 EmptyTree EmptyTree) EmptyTree
λ> :t result
result :: Num b => Tree b
```

`pure (+) <*> tree1 <*> tree2` could be intuitively visualized like this:

```
    pure (+) <*> tree1      <*>       tree 2       ->       result

          (+)1                          5                    6
          /  \                         /                    /
         /    \                       /                    /
      (+)2   (+)3                    6                    8

```

---------------------------

### Monoid

In the section of Applicative, we learnt how a series of functors could be passed as arguments to a regular function. 
Now, let's consider a similar but slightly different problem: 
how can we apply a function to reduce or fold a tree recursively?
e.g. calculating the sum or product of values in a tree. 


There are many of ways to implement this in python. Here is one way:

```python
def reduce_tree(tree, func, default=1):
    if tree:
        reduced_left = func(reduce_tree(tree.left, func, default=default), tree.val)
        reduced = func(reduced_left, reduce_tree(tree.right, func, default=default))
        return reduced
    else:
        return default
```

```python
>>> tree = Tree(val=1, left=Tree(val=2), right=Tree(val=4))
>>> reduce_tree(tree, lambda a,b: a+b, default=0)
7
>>> reduce_tree(tree, lambda a,b: a*b, default=1)
8
>>> reduce_tree(tree, min, default=float("inf"))
1
>>> reduce_tree(tree, max, default=-float("inf"))
4
```

We can also make the `Tree` iterable, so we can directly use `reduce` and make our code a bit more functional.

```python
class Tree:
    def __init__(self, left=None, right=None, val=0):
        self.left = left
        self.right = right
        self.val = val

    def __repr__(self):
        return f'Tree(val={self.val}, left={self.left}, right={self.right})'

    def __iter__(self):
        if self.left:
            yield from self.left
        yield self.val
        if self.right:
            yield from self.right
```

```python
>>> tree = Tree(val=1, left=Tree(val=2), right=Tree(val=4))
>>> list(tree)
[2, 1, 4]
>>> from functools import reduce
>>> reduce(lambda a,b: a+b, tree, 0)
7
>>> reduce(lambda a,b: a*b, tree, 1)
8
>>> reduce(min, tree, float("inf"))
1
>>> reduce(max, tree, -float("inf"))
4
```

There is an important assumption in implementations above. That is, all functions being applied to the tree has 
to be [associative](https://en.wikipedia.org/wiki/Associative_property). Associativity means that the order of function 
application doesn't determine the final output value. To define it more strictly, 
a binary function `f` is associative if the following condition satisfy for all `x`, `y`, `z` in S, 
where S is a set of values. 
 
```
f(x, f(y, z)) = f(f(x,y), z)
```

It is not hard to tell that `+`, `*`, `min`, and `max` are all associative. That's why we can apply them to a Tree 
without worrying about the order in which they are applied internally.

An example of non-associative function is average. e.g. `average(average(a, b), c)` is not equal to 
`average(a, average(b, c))`.
If we apply average to a tree, the final value will be different depending on whether left child or right child is 
reduced with the root first.

```python
def reduce_tree(tree, func, default=1):
    if tree:
        reduced_left = func(reduce_tree(tree.left, func, default=default), tree.val)
        reduced = func(reduced_left, reduce_tree(tree.right, func, default=default))
        return reduced
    else:
        return default

def reduce_tree2(tree, func, default=1):
    if tree:
        reduced_right = func(tree.val, reduce_tree2(tree.right, func, default=default))
        reduced = func(reduce_tree2(tree.left, func, default=default), reduced_right)
        return reduced
    else:
        return default
``` 

```python
>>> tree
Tree(val=1, left=Tree(val=2, left=None, right=None), right=Tree(val=4, left=None, right=None))
# Addition is not determined by the order in which values are applied
>>> reduce_tree(tree, lambda a,b: a+b, default=0)
7
>>> reduce_tree2(tree, lambda a,b: a+b, default=0)
7
# Average is determined by the order in which values are applied
>>> reduce_tree(tree, lambda a,b: (a+b)/2, default=0)
0.875
>>> reduce_tree2(tree, lambda a,b: (a+b)/2, default=0)
0.75
```

Why are we talking about associativity? Because it is the essence of monoids.
In Haskell, Monoid is a type class that has an associative binary function and an identity value, `A`, which, when 
applied the binary function along with any value, `B`, always yields `B` as the output.

For example, `0` is the identity value with respect to binary function `+`, and `1` is the identity value with respect 
to binary function `*`.

Here is the formal definition of Monoid type class:

```haskell
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty  
```

`mempty` is the identity value, and `mappend` is the associative binary function.  
`mconcat` is a function that "concatenates" a list of monoids into one, and we get it for free with a default 
implementation.

Now, let's first see how to make a foldable tree in haskell *without* using monoids.

```haskell
import qualified Data.Foldable as F  

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldr f z EmptyTree = z
    foldr f z (Node val left right) = f (f val rightResult) leftResult
	    where
	    rightResult = foldr f right z
	    leftResult = foldr f left z
```

The code seems fine in the first look. We pass the reduced results from right and left to function `f`, and we get the 
final reduced result . 
However, GHC complains!
```
    • Couldn't match expected type ‘a’ with actual type ‘b’

  |
7 |     foldr f z (Node val left right) = f (f val rightResult) leftResult
  |                                          ^^^^^^^^^^^^^^^^^
``` 
Well, it turns that function `f` (whose type is `a -> b -> b`) is expecting its first argument to be type `a`, but it got 
type `b`, which is the output of `f val rightResult`. In fact, we don't really have a way of turning type `b` into 
type `a`. We are stuck here.

Fortunately, `Foldable` provides a second interface, `foldMap`, which is easier to implement:
```haskell
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  
```

Now, instead of taking a function of type `a -> b -> b`, `foldMap` takes a function of type `a -> m`, which transforms 
a type, `a`, to a monoid, `m`.

Here is the implementation of foldMap for Tree:

```haskell
instance F.Foldable Tree where  
    foldMap f EmptyTree = mempty  
    foldMap f (Node x left right) = mconcat [foldMap f l, f x, foldMap f r]
```

Now we can see the advantage of using monoid here. 
`F.foldMap f left`, `f x`, and `F.foldMap f right` all return a monoid as outputs, which could be "concatenated" into  
a single monoid with `mconcat`. Now, we just need to find a function that turns a value of a Node to a monoid.

A simple way is to create a newtype as a monoid for numbers.  

```haskell
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)  

instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)  
``` 

```haskell
λ> getProduct $ Product 3 `mappend` Product 4
12
```

Now, we can use `Product` as a monoid for foldMap.
```haskell
λ> tree = Node 2 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)
λ> getProduct $ foldMap Product tree
8
```

Not surprisingly, `Product` is defined as one of the monoids in standard module `Data.Monoid`, along with Sum, Min, Max, 
and many more.

```haskell
λ> getSum $ foldMap Sum tree
7
```

---------------------------

### Monad

[Monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)) is one of the most important concepts in 
functional programming, but it is not well known in the realm of object-oriented programming. 
Nevertheless, we will try to implement a monad in Python, use it in an example, and then show how it could be 
equivalently implemented and used in Haskell.

Programs can fail with exceptions, which can usually cause side effects in control flows. 

For example, in Python, a division function will throw a `ZeroDivisionError` when the divisor is 0. 

```python
>>> def div(x, y):
...     return x / y
...
>>> div(4, 2)
2.0
>>> div(3, 2)
1.5
>>> div(3, 0)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 2, in div
ZeroDivisionError: division by zero
```

When a `ZeroDivisionError` exception is thrown and not handled within `div`, the exception will be propagated to the 
caller of `div`, causing a side effect. If we want to get rid of such side effect, or in other words, 
make it a [pure](https://en.wikipedia.org/wiki/Pure_function) function, we need to catch and handle any possible 
exception within it. 
This could be implemented by creating a wrapper class. Let's call it `Maybe`, which stores a successfully returned 
value or a "failure" state when an exception is thrown. We also need a wrapper function that safely calls the division  
function and wraps the returned value into the wrapper. See the implementation below.

```python
class Maybe():
    def __init__(self, value=None, failed=False):
        self.value = value
        self.failed = failed

    def __repr__(self):
        if self.failed:
            return "Nothing"
        else:
            return f"Just {self.value}"

def safe_div(x, y):
    try:
        value = x / y
        return Maybe(value=value)
    except Exception as e:
        print(f"Got exception: {e}") # IO is also a side effect, but we will keep it here to help us understand
                                     # the behavior of this function in examples below.
        return Maybe(failed=True)
```

With class `Maybe` and function `safe_div`, we can both eliminate its side effects and retain its pureness.

```python
>>> safe_div(4, 2)
Just 2.0
>>> safe_div(4, 1)
Just 4.0
>>> safe_div(4, 0)
Got exception: division by zero
Nothing
```

However, with this design, we will inevitably run into a problem when more than one `safe_div` are chained together:

```
>>> x = safe_div(4, 2)
>>> y = safe_div(2, 1)
>>> x
Just 2.0
>>> y
Just 2.0
>>> safe_div(x, y)
Got exception: unsupported operand type(s) for /: 'Maybe' and 'Maybe'
Nothing
```

`safe_div(x, y)` is supposed to return `Just 1.0`, but it ends up returning `Nothing`! 
The problem is that both `x` and `y` returned from `safe_div` are in type `Maybe`, but operator "`/`" doesn't know how 
to operate on `Maybe`, so an exception is thrown by `div` and caught inside `safe_div`, which returned `Nothing`. 

A simple way to fix this is to create another function `safe_div_maybe`, which takes inputs in `Maybe` types and calls 
`safe_div` internally.

```python
def safe_div_maybe(x: Maybe, y: Maybe) -> Maybe:
    if not x.failed and not y.failed:
        return safe_div(x.value, y.value)
    else:
        return Maybe(failed=True)
```

```python
>>> x = safe_div(4, 2)
>>> y = safe_div(2, 1)
>>> x
Just 2.0
>>> y
Just 2.0
>>> safe_div_maybe(x, y)
Just 1.0
```

However, with this approach, we need to create two functions (`safe_div` and `safe_div_maybe`) that do pretty 
much the same thing except that their input types are different. In addition, for every new function, e.g. `func_x`, 
we will need to create `safe_func_x` and its counter part `safe_func_x_maybe`. This doesn't seem to be an elegant 
solution.

The root problem is that function `safe_div` is not composable, because its input type (`int`) and 
its output type (`Maybe`) are different.
Therefore, we cannot simply pass an output value from a `safe_div` as an input argument to another `safe_div`. 
In comparison, the original operator "`/`" is composable, because we can pass the output value from a "`/`" to another 
"`/`" without any problem, as shown below:

```python
>>> x = 4 / 2
>>> y = 2 / 1
>>> x / y
1.0
```

Is there a wonderland where we can enjoy both the composability of "`/`" and the pureness of `safe_div`? 
Yes, this is where monad shines.

Let's create a magical function, named "bind".

```python
from typing import Callable

def bind(x: Maybe, some_safe_call: Callable[..., Maybe]) -> Maybe:
    if x.failed:
        return Maybe(failed=True)
    else:
        return some_safe_call(x.value)
```

`bind` takes two inputs: a Maybe object, and a function that takes a raw value and returns a Maybe object. 
First, it checks if the value stored in the first input is failed. If so, it simply returns 
a failed `Maybe`. Otherwise, it will pass its internally stored value to function `some_safe_call`, and directly return 
the output of it. With `bind`, we can now chain multiple `safe_div`'s together:

```python
>>> maybe_x = safe_div(4, 2)
>>> maybe_y = safe_div(4, 1)
>>> bind(maybe_x, lambda x: bind(maybe_y, lambda y: safe_div(x, y)))
Just 0.5
```

How does it work? Let's first look at the most inner call `safe_div(x, y)`. We are already familiar with this. 
`x` and `y` are passed to `safe_div` as raw integers (not `Maybe` type). 
The more interesting part is the two lambda functions introduced. 
The first `bind` is called with input `maybe_x` and a lambda function. 
The purpose of the lambda function is to, by using `bind`, create an environment/context where the actual value `x` 
could be extracted from `maybe_x` and passed to function `safe_div`. Here, `bind` could also be seen as a variable 
assignment, which assigns the actual value stored in `maybe_x` to a temporary variable `x`. 
The same idea also applies to `maybe_y`, whose actual value is extracted by the second `bind` and passed along with `x` 
to `safe_div`.

Now, if we tweak the code with some new lines in between, it actually looks imperative in a way that each `bind` call 
acts like a reversed variable assignment, where the variable shows after its assigned value. This format is not 
a good style but it helps us to understand the binding relations. 

```python
bind(         maybe_x,      lambda x:
  ^              ^                 ^
assignment     value           variable
```

```python
>>> maybe_x = safe_div(4, 2) # Just 2
>>> maybe_y = safe_div(4, 1) # Just 4
>>> bind(maybe_x, lambda x:  # bind the value stored in maybe_x to x
... bind(maybe_y, lambda y:  # bind the value stored in maybe_y to y
... safe_div(x, y)           # x=2 and y=4 are passed to safe_div
... )                        # ) matches ( from second bind
... )                        # ) matches ( from first bind
Just 0.5

# Notice that we are free to change the order in which "x" and "y" are defined

>>> maybe_x = safe_div(4, 2) # Just 2
>>> maybe_y = safe_div(4, 1) # Just 4
>>> bind(maybe_y, lambda y:  # bind the value stored in maybe_y to y (y <- 4)
... bind(maybe_x, lambda x:  # bind the value stored in maybe_x to x (x <- 2)
... safe_div(x, y)           # x=2 and y=4 are passed to safe_div
... )
... )
Just 0.5
```

With the magic function `bind`, we are now able to compose any number of `safe_div` together.

More examples:

```python
>>> maybe_x = safe_div(4, 2) # Just 2
>>> maybe_y = safe_div(4, 1) # Just 4
>>> maybe_z = safe_div(1, 2) # Just 0.5
>>> maybe_n = safe_div(1, 0) # Nothing
Got exception: division by zero
>>> bind(maybe_n, lambda n:  # bind the value stored in maybe_n to n
... bind(maybe_y, lambda y:  # bind the value stored in maybe_y to y
... safe_div(n, y)
... )
... )
Nothing

>>> bind(maybe_x, lambda x:                 # bind the value stored in maybe_x to x
... bind(maybe_y, lambda y:                 # bind the value stored in maybe_y to y
... bind(maybe_z, lambda z:                 # bind the value stored in maybe_z to z
... bind(safe_div(y, x), lambda y_div_x:    # bind the value stored in the output of safe_div(y, x) to y_div_x
... safe_div(y_div_x, z)
... )
... )
... )
... )
Just 4.0
```

In all examples above, whenever a safe function, e.g. `safe_div`, need to access the raw value stored in a maybe object 
as its input, we can create a lambda function, whose input is "bind" to the raw value, and pass the function along 
with the Maybe object to `bind`, as a pattern looks roughly like this: `bind(maybe_x, lambda x: ...)`. 

`bind` is what makes Maybe a monad. It brings composability to `safe_div`, and essentially any functions that takes 
raw value as inputs and returns `Maybe` outputs.

Now that we have a good understanding of monad, and saw its implementation in Python, let's look at how it is 
implemented in Haskell.

First, let's create a `Maybe` data type in Haskell.

```haskell
data Maybe a = Just a | Nothing deriving Show
```

In fact, `Maybe` is already implemented by Haskell's standard library and imported as a part of prelude.

`Maybe` has two constructors, `Just` and `Nothing`. `Just` takes one value, which could be any type.
`Nothing` does not have input argument.

```haskell
λ> Just 1
Just 1
λ> Just 2
Just 2
λ> Nothing
Nothing
```

With `Maybe`, we can create a function that performs division and returns a `Maybe` type, and use `Nothing` to represent 
the failed status of an operation.

```haskell
safeDiv Int :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
```

The implementation is a bit different from Python example. In Python, we use `try...except` to catch exceptions. 
In Haskell, we directly check the value of divisor before dividing the numbers to avoid any exception.
The purpose is to simplify the Haskell example so readers can focus more on the concept of Monad.

Now, let's try a couple of examples.

```haskell
λ> safeDiv 4 2
Just 2
λ> safeDiv 4 1
Just 4
λ> safeDiv 4 0
Nothing
```

If we pass `Maybe` types to `safeDiv`, Haskell will complain:

```haskell
λ> maybeX = safeDiv 4 2
λ> maybeY = safeDiv 4 1
λ> safeDiv maybeX maybeY

<interactive>:14:9: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Maybe Int’
    • In the first argument of ‘safeDiv’, namely ‘maybeX’
      In the expression: safeDiv maybeX maybeY
      In an equation for ‘it’: it = safeDiv maybeX maybeY

<interactive>:14:16: error:
    • Couldn't match expected type ‘Int’ with actual type ‘Maybe Int’
    • In the second argument of ‘safeDiv’, namely ‘maybeY’
      In the expression: safeDiv maybeX maybeY
      In an equation for ‘it’: it = safeDiv maybeX maybeY
```

This is because `safeDiv` expects input type `Int`, but the actual input type `Maybe Int`. This is exactly the same 
problem we encountered in Python example, except that the error is thrown because of mismatched types.

We will use the same solution to solve this problem by introducing the `bind` function.

Let's define Monad more rigidly as a type class.

```haskell
class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
```

Symbol "`(>>=)`" is called "bind" in haskell. It has the exactly same purpose as `bind` function in our previous 
Python example.
"`(>>=)`" takes two inputs. The first input is of type "`m a`", which will be matched to "Maybe Int" in the context of 
our example. The second input is a function, whose type is `(a -> m b)`. In our case, it will be a lambda function.

```haskell
instance Monad Maybe where
    Nothing  >>= f = Nothing
    (Just x) >>= f = f x
```

The implementation is almost identical to the one we saw in Python. We check if the first object is "Nothing", 
if so, we ignore the second argument, function `f`, and directly return "Nothing". Otherwise, we take the actual value 
stored in the first input, and pass it to function `f` and return the result.

Now, let's try to use "`(>>=)`".

```haskell
λ> maybeX = safeDiv 4 2
λ> maybeY = safeDiv 4 1
λ> maybe_x >>= (\x -> (maybe_y >>= (\y -> safeDiv y x)))
Just 2
```

It seems to work! We are basically doing the same thing as we did in Python examples.
Comparing the code side by side:

|   |  |
| --- | --- |
| Python  | `bind(maybe_x, lambda x: bind(maybe_y, lambda y: safe_div(y, x)))` |
| Haskell | `maybe_x >>= (\x -> (maybe_y >>= (\y -> safeDiv y x)))`            |
|  |  |

The haskell's expression could be written in multiple lines when wrapped in a function:

```haskell
doWork :: Maybe Int -> Maybe Int -> Maybe Int
doWork maybeX maybeY = maybeX >>= \x ->
                       maybeY >>= \y ->
                       safeDiv x y
```

```haskell
λ> maybeX = safeDiv 4 2
λ> maybeY = safeDiv 4 1
λ> doWork maybeY maybeX
Just 2
```

Does it look familiar? Yes, it looks really close to our Python implementation except that function "`bind`" is 
now replaced with the infix operator "`(>>=)`".

Because binding is used quite often in Haskell, the language provides a syntax sugar to free us from 
creating lambda functions and typing `(>>=)` over and over again. The syntax sugar starts with a Haskell keyword 
"`do`" and followed by expressions of "`a <- Ma`":

```haskell
doWork :: Maybe Int -> Maybe Int -> Maybe Int
doWork maybeX maybeY = do
    x <- maybeX
    y <- maybeY
    safeDiv x y
```

```haskell
λ> maybeX = safeDiv 4 2
λ> maybeY = safeDiv 4 1
λ> doWork maybeY maybeX
Just 2
```
