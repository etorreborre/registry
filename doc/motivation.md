# Motivation


#### Functions

What is the best software component?

> a function?

Yes indeed! A function is like a closed box with a nice label on top declaring exactly what it is doing. It is also a **sealed** box so the provider of the function can change the implementation without you having to change any of your code:

```haskell
square :: Int -> Int
square n = n * n -- who knew?
```

A higher-order function is even better. You can pass another function to alter the behavior of the first one:

```haskell
squareOkOrZero :: (Int -> Bool) -> Int -> Int
squareOkOrZero check n =
  let squared = square n
  in  if check n then squared else 0
```

So functions are like boxes inside boxes inside boxes,... and the top-level box is totally sealed. This is fantastic for modularity because this hides low-level information.

#### The problem

This is also desastrous for reuse because we sometimes want to open the box, rearrange slightly the inside,and shut the box again. Really? Yes.

##### Encoders

For example we can use an `Encoder` to describe how a `Company` can be serialized to `JSON`:
```haskell
data Company    = Company    { departments :: [Department] }
data Department = Department { employees :: [Employee] }
data Employee   = Employee   { name :: Name, age :: Age }

newtype Name = Name Text
newtype Age = Age Int

-- this code uses a fictive `JSON` library providing functions to create JSON values
-- string, number, obj, arr, .=

name :: Name -> JSON
nameEncoder (Name n) = string a

-- other signature are omitted
ageEncoder (Age a) = number a

employeeEncoder   (Employee n a)  = obj ["n" .= nameEncoder a, "a" .= ageEncode a]
departmentEncoder (Department es) = obj ["employees" .= arr (employeeEncoder <$> es)]
companyEncoder    (Company ds)    = obj ["department" .= arr (departmentEncoder <$> ds)]
```

Once given a `companyEncoder` you can encode any `Company`, great! However you are restricted to just one implementation. If you want to change some of the field names, for example use better fields names for the `employeeEncoder`, `name` and `age` instead of `n` and `a`, the best you can do is redefine your encoders:
```haskell
employeeEncoder' (Employee n a)  =
  obj ["name" .= nameEncoder a, "age" .= ageEncode a]

departmentEncoder' empEncoder (Department es) =
  obj ["employees" .= arr (empEncoder <$> es)]

companyEncoder' dptEncoder (Company ds) =
  obj ["department" .= arr (dptEncoder <$> ds)
```

Then you can define
```haskell
myCompanyEncoder' =
  companyEncoder' (departmentEncoder' employeeEncoder')
```
Which means that you need to manually assemble all the encoders you will need. There are of course other solutions to this issue, relying on type classes and/or TemplateHaskell. They have similar drawbacks, for example there can only be one encoder for the `Employee` type (using newtypes might be impossible if that data structure comes from a library).

This issue happens in other contexts, when using Hedgehog generators for instance, but it is especially present when trying to structure applications as a set of "components", which is the main use case for this library.

##### Applications

When building medium to large applications it is very tempting to start grouping related functions together when they share the same implementation or the same configuration. For example when saving data to a database:
```haskell

module Acme.CompanyRepository where

import qualified Acme.Logging as Logging

data Module = Module {
  saveCompany    :: Company -> IO ()
, getCompanies   :: IO [Company]
, getCompanyById :: Text -> IO (Maybe Company)
}

data Config = Config {
  host :: Text
, port :: Int
}

new :: Config -> Logging.Module -> Module
new config logging = Module
  {- implement saveCompany  -}
  {- implement getCompanies  -}
  {- implement getCompanyById  -}
```

In the code above `new` is a constructor for a `CompanyRepository.Module` and uses some configuration and a `Logging` component. If you scale this approach to a full application you end up in the situation described for encoders where you need to manually call several functions to create the full structure. You will also need to parametrize those functions so that you can create different versions of the application for different environments: production, staging, development...
```haskell

logging = Logging.new

companyRepository =
   CompanyRepository.new
     (CompanyRepository.Config "host" 5432 logging)

-- | more definitions...

-- | the full application
app =
  App.new logging
    companyRepository
    impageProcessing
    s3Access
    -- ...
```

##### The solution

In summary there are advantages to manually assembling functions:

 - we don't need any fancy typelevel technique, just good old Haskell 98 functions
 - we are as flexible as we want and can specify exactly which behaviour is needed where
 - unit testing is straightforward, just call a function directly with its arguments

But there are obvious drawbacks:

 - this code is tedious to write
 - it impedes refactoring because a simple change in the structure of your data model or application can trigger many changes

The solution? Abstract over the construction process in order to modify it to suit our needs. This library provides a simple data structure, a [`Registry`](./registry.md), and a "resolution" algorithm to encode the assembly of functions and modify it if necessary.
