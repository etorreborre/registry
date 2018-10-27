# Generators

[Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) generators are a very nice way to describe how to generate random instances of a given datastructure. Let's take the `Company`/`Department`/`Employee` example that we've seen in [the encoders section](./encoders) and create some generators:
```haskell
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range

genName :: Gen Name
genName = Gen.text (Range.linear 2 10) Gen.ascii

genAge :: Gen Age
genAge = Gen.int (Range.linear 18 65)

genEmployee :: Gen Employee
genEmployee = Employee <$> genName <*> genAge

genDepartment :: Gen Department
genDepartment = Department (Gen.list (Range.linear 1 10) genEmployee)

genCompany :: Gen Company
genCompany = Company (Gen.list (Range.linear 1 5) genDepartment)
```

This is already useful because this lets us generate companies of various sizes in terms of number of departments and employees. This is not a panacea though. For example with the code above we could generate a company where the same employee belongs to 2 different departments!

What is also annoying is that we cannot easily generate a company with just one `Department` with one `Employee`. If we want to do this we have to:

 - parameterize the `genCompany` function to accept a `genDepartment`
 - parameterize the `genDepartment` function to accept a `genEmployee`
 - write some code passing the right `genEmployee` to the right `genDepartment` to the new `genCompany`

Fortunately the Registry is here to help us with all of that!

#### In a registry

Consider the constructor for `Company` for a second
```haskell
Company :: [Department] -> Company
```
If we "lift" it into the `Gen` monad we get
```haskell
Company :: Gen [Department] -> Gen Company
```
This is precisely what the `funTo` combinator does, so let's build a registry with all of our lifted constructors:
```haskell
registry =
     funTo @Gen Company
  +: funTo @Gen Department
  +: funTo @Gen Employee
  +: funTo @Gen Name
  +: funTo @Gen Age
  +: end
```

Not quite right. With the registry above we cannot build a `Gen Age` because the constructor for `Age` requires some `Int`, and the constructor for `Name` requires some `Text`. Let's add those generators to the registry:
```haskell
genText = Gen.text (Range.linear 2 10) Gen.ascii
genInt = Gen.int (Range.linear 18 65)

registry =
     funTo @Gen Company
  +: funTo @Gen Department
  +: funTo @Gen Employee
  +: funTo @Gen Name
  +: funTo @Gen Age
  +: fun   genText -- no lifting needed here
  +: fun   genInt  -- no lifting needed here
  +: end
```
We are almost there. When we want to build a `Gen Departement` we need a `Gen [Employee]` but the current registry only contains `Gen Employee`. Fortunately we have a function `Gen a -> Gen [a]`:
```haskell
genList = Gen.list . Range.linear 1 5
```
(similarly we could introduce `genMaybe`, `genNonEmpty`, `genSet`,...)

So we can now complete our registry:
```haskell
registry =
     funTo @Gen (genList @Department)
  +: funTo @Gen (genList @Employee)
  +: funTo @Gen Company
  +: funTo @Gen Department
  +: funTo @Gen Employee
  +: funTo @Gen Name
  +: funTo @Gen Age
  +: fun   genText
  +: fun   genInt
  +: end
```
And finally
```haskell
company :: Gen Company
company = make @(Gen Company) registry
```
Which we can even abbreviate with a helper function `gen`:
```haskell

-- | In your tests you might want to remove the Solvable constraint and use `makeFast`
--   for better compile times or even also remove `Contains a out` and use `makeUnsafe`
--   and rely on runtime error messages for precise diagnostics when something is
--   missing from the registry
gen :: forall a ins out. (Typeable a, Contains a out, Solvable ins out) => Gen a
gen = make @(Gen a) registry

company :: Gen Company
company = gen @Company
```

You can also add useful other generator functions for lists, maybes,...
```haskell
genMaybe :: forall a . (Typeable a) => Gen a -> Gen (Maybe a)
genMaybe = Gen.maybe

genList :: forall a . (Typeable a) => Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 3)

genNonEmpty :: forall a . (Typeable a) => Gen a -> Gen (NonEmpty a)
genNonEmpty = Gen.nonEmpty (Range.linear 1 3)
```

#### Add generation constraints

How do we generate companies with just one department of one employee from there? We can use the `Registry.tweak` function:
```haskell
-- | reminder :- is an alias for "Contains"
setOneEmployee :: (out :- Gen [Employee]) => Registry ins out -> Registry ins out
setOneEmployee = tweak @(Gen [Employee]) (take 1 <$>)
```
The code above says:

 - once you have created a `Gen [Employee]` generator apply a function to it
 - we apply the function `take 1 <$>` which leaves just one employee in the list
   (this works because the initial generator for a list of employees puts at least one employee in the list)

We can say the same thing for departments:
```haskell
setOneDepartment :: (out :- Gen [Department]) => Registry ins out -> Registry ins out
setOneDepartment = tweak @(Gen [Department]) (take 1 <$>)
```

And since `setOneEmployee` and `setOneDepartment` are just functions modifying a `Registry` we can compose them:
```haskell
setMinimalCompany
  :: (out :- Gen [Department],
      out :- Gen [Department])
  => Registry ins out
  -> Registry ins out
setMinimalCompany =
  setOneEmployee .
  setOneDepartment
```

When we eventually want such a company
```haskell
-- two more helper functions
tweakGenWith :: forall a ins out . (Typeable a, out :- a)
  => Registry ins out
  -> (Registry ins out -> Registry ins out)
  -> Gen a
tweakGenWith registry registryModification =
  makeFast @(Gen a) (registryModification registry)

tweakGen = tweakGenWith registry

minimalCompany :: Gen Company
minimalCompany = tweakGen setMinimalCompany
```

#### Generate data for an ADT

Creating generators for an ADT is a bit trickier. Indeed an ADT offers several constructors for the same
type:
```
data Salary =
    Fixed Int
  | Variable Int Double -- a fixed part and a percentage of annual sales
```

If we put the 2 constructors, `Gen Fixed` and `Gen Variable` in the registry, only the first one will be
used to create a `Gen Salary` value. However we can do the following:
```
genSalary :: Gen Bool -> Gen (Tagged 1 Fixed) -> Gen (Tagged 2 Variable) -> Gen Salary
genSalary genBool
```

#### Summary

What we get with this approach is:

 - a minimal way to create generators from datatypes by
     - putting their constructors in a registry
     - creating generators for the "leaves" of the data structure (like `genText` and `genInt`)

 - a composable way to define constraints to apply to those generators

 - the possibility to specialize the generators to use depending on which part of the data structure we are building with `Registry.specialize`. For example if `Departments` had a `Name` we could specify a `Gen Name` to use which would be different from the one used for generating `Employees` (see .applications.md for a more in-depth example)
