# The Registry

#### The resolution algorithm

Let's imagine for a moment that you have a stack of functions to create "output values". You also put on your stack some "input" values. If you want a value of a given type you can:

 1. go through the list of existing values and if you find one with the desired type return it
 1. otherwise try to find a function returning a value of the given type
 1. if you find such a function apply the same algorithm to build all its input values
 1. every newly built value is put on top of the stack so it is available as an input to another function

You can eventually create a value out of the Registry if:

 - the value type is one of the existing values types
 - or if its type is the output type of one of the functions
 - the function inputs types are also existing value types or output types of other functions
 - there are no cycles!

#### A small example

Let's use a `Registry` to deal with the "encoders" example given in the [motivation](./motivation.md) section. We need first to introduce the type of encoders, `Encoder`:

```haskell
data Encoder a = Encoder { encode :: a -> JSON }
```

Then we can define a list of encoders and encoder functions:

```haskell
nameEncoder = Encoder { encode (Name n) = string n }
ageEncoder = Encoder { encode (Age a) = number a }

employeeEncoder nameE ageE = Encoder {
  encode (Employee n a)  = obj ["n" .= nameE n  , "a" .= ageE a]
}

departmentEncoder employeeE = Encoder {
  encode (Department es) = obj ["employees" .= arr (employeeE <$> es)]
}

companyEncoder departmentE = Encoder {
  encode (Company ds) = obj ["department" .= arr (departmentE <$> ds)
}
```

We can already see something interesting. The right levels of abstraction are respected because the `departmentEncoder` doesn't have to know how the `employeeEncoder` is implemented for example.

Now we put everything in a `Registry`
```haskell
import Data.Registry

registry =
     fun nameEncoder
  +: fun ageEncoder
  +: fun employeeEncoder
  +: fun departmentEncoder
  +: fun companyEncoder
  +: end
```

In the code above `end` is the "empty" registry and `+:` adds a new element to the registry. `fun` is a function which helps us describe registry elements which can only be described with their type information like functions (they have a `Typeable` instance), versus `val` which represents elements which can also be displayed in full (they have a `Show` instance). See the [Reference guide](./reference.md) for a list of all those functions.

With a `registry` we can ask to make any encoder
```haskell
-- enable {-# LANGUAGE TypeApplications #-}

nameEncoder1    = make @(Encoder Name) registry
companyEncoder1 = make @(Encoder Company) registry
```

Can we produce an `Encoder Company` where all the names will be capitalized? Yes, by adding another `Encoder Name` on top of the existing one in the registry:
```haskell

nameCapitalizedEncoder = Encoder {
  encode (Name n) = (nameEncoder & encode) (Name (capitalize n))
}

registry' = fun nameCapitalizedEncoder +: registry

companyEncoder2 = make @(Encoder Company) registry'
```

Since the resolution algorithm looks for values "top to bottom" on the registry stack it will find `nameCapitalizedEncoder` to be used when building other encoders.

That's all it takes! Now you can have a look at the main reason for this library to exist: how to build [applications](./applications.md).
