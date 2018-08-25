# Boxes

It's boxes all the way down.

# Motivation

Wouldn't it be nice to be able to build applications, programs out of
small elements which can be easily assembled together? Yes indeed
and this is a premise of Functional Programming. Those small elements
are functions and we can compose them to get "bigger" functions:
```haskell
add1 :: Int -> Int
add1 n = n + 1

square :: Int -> Int
square n = n * n

add1AndSquare :: Int -> Int
add1AndSquare = square . add1
```

Functions are the ultimate "software component". They define their interface
precisely and are completely sealed, you can not modify their implementation.

This is also their biggest drawback! Sometimes it is very desirable to slightly
tweak the implementation of a function:

 1. when building a large application, if you want to change a parameter or
    a function used by the top level one

 2. when using data generators if you want to modify the generation of parts
    of a bigger datastructure

 3. when using json encoders if you want to modify the encoding of parts
    of a bigger datastructure

# Solution

The solution presented here is very simple and can be extended to solve
more complex use cases. We will present it in terms of building an application
where each "component" is a record of functions.

The idea is to build a "registry" of values and functions necessary to
build components of a given type. If you want to build a component

  1. we look at the first available value of that type
  2. if already present in the registry we return it
  3. if not we look for a function returning such a value
  4. if such a function exists we try to create its input values
     from the registry recursively by using the same algorithm.
  5. we then apply the function to the input values and we put
     the result on top of the registry for future uses (so that
     results are memoized and components become singletons in an
     application graph)

A small example of creating a registry and using it to create an
application can be found [here](test/Test/Data/Box/SmallExample.hs).
