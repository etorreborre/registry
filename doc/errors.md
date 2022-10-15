# How to read type errors

The `<:` operator is very useful to add a function to a registry since it checks that the function inputs
can be produced from the registry. However it can also produce some confusing or long error messages.

Here are some tips and tricks to read those messages.

## Adding a polymorphic function to the registry

A registry requires that we only add fully monomorphic functions.
For example, this registry will produce an error message
```haskell
load :: Monad m => a -> m a
load = pure

r :: Registry _ _
r =
     fun load
  <: val (1 :: Double)
  <: val (1 :: Int)
  <: val ("1" :: Text)
  <: val ('1' :: Char)
  <: val (1 :: Double)
```

The error is
```
   • Could not deduce: (Inputs (m0 a) :++ '[])
                        ~ (Inputs (m a) :++ '[])
      from the context: (CanMake
                           a '[Double, Int, Text, Char, Double] (a -> m a),
                         Monad m, Typeable m, Typeable a,
                         IsSubset
                           (Inputs (m a)) '[Double, Int, Text, Char, Double] (a -> m a))
        bound by the inferred type for ‘r’:
                   forall {a} {m :: * -> *}.
                   (CanMake a '[Double, Int, Text, Char, Double] (a -> m a), Monad m,
                    Typeable m, Typeable a,
                    IsSubset
                      (Inputs (m a)) '[Double, Int, Text, Char, Double] (a -> m a)) =>
                   Registry
                     (a : (Inputs (m a) :++ '[]))
                     '[Output (m a), Double, Int, Text, Char, Double]
        at ...
      Expected: Registry
                  (a : (Inputs (m a) :++ '[]))
                  '[Output (m a), Double, Int, Text, Char, Double]
        Actual: Registry
                  (a : (Inputs (m0 a) :++ '[]))
                  '[Output (m0 a), Double, Int, Text, Char, Double]
      NB: ‘:++’ is a non-injective type family
      The type variable ‘m0’ is ambiguous
    • In the ambiguity check for the inferred type for ‘r’
      To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
      When checking the inferred type
        r :: forall {a} {m :: * -> *}.
             (CanMake a '[Double, Int, Text, Char, Double] (a -> m a), Monad m,
              Typeable m, Typeable a,
              IsSubset
                (Inputs (m a)) '[Double, Int, Text, Char, Double] (a -> m a)) =>
             Registry
               (a : (Inputs (m a) :++ '[]))
               '[Output (m a), Double, Int, Text, Char, Double]
```

And the error grows larger as the registry gets more type variables.

In general the message to catch is `The type variable ‘m0’ is ambiguous`. It is telling you that some function is too polymorphic and
one or more of its type parameters must be specified. Like so:
```haskell
r :: Registry _ _
r =
     fun (load @IO @Int)
  <: val (1 :: Double)
  <: val (1 :: Int)
  <: val ("1" :: Text)
  <: val ('1' :: Char)
  <: val (1 :: Double)
```

## Forgetting `fun`

If we modify slightly the previous example and remove the call to `fun` on the first line:
```haskell
r :: Registry _ _
r =
      (load @IO @Int)
    <: val (1 :: Double)
    <: val (1 :: Int)
    <: val ("1" :: Text)
    <: val ('1' :: Char)
    <: val (1 :: Double)
```
Then the compiler tells us that:
```
• No instance for (AddRegistryLike
                         (Int -> IO Int)
                         (Registry '[] '[Double, Int, Text, Char, Double])
                         (Registry w1 w0))
        arising from a use of ‘<:’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression:
        (load @IO @Int)
          <:
            val (1 :: Double)
              <:
                val (1 :: Int)
                  <: val ("1" :: Text) <: val ('1' :: Char) <: val (1 :: Double)
      In an equation for ‘r’:
          r = (load @IO @Int)
                <:
                  val (1 :: Double)
                    <:
                      val (1 :: Int)
                        <: val ("1" :: Text) <: val ('1' :: Char) <: val (1 :: Double)
    |
143 |     <: val (1 :: Double)
```
Again this error message can be rather daunting if the registry is large.
In this case you can look for `AddRegistryLike` which is the typeclass behind the scenes of the `<:` operator.
The error message tells us that what we are trying to add is not very "AddRegistryLike" and we can fix
it by adding `fun`.

## Adding a function with missing inputs

Let's use our example again:
```haskell
r :: Registry _ _
r =
     fun (load @IO @Bool)
  <: val (1 :: Double)
  <: val (1 :: Int)
  <: val ("1" :: Text)
  <: val ('1' :: Char)
  <: val (1 :: Double)
````
This time the error message is more explicit:
```
   • The function creating the output type

        IO Bool

      cannot be added to the registry because the input parameter

        Bool

       is not one of the registry outputs

      The full function type for IO Bool is

      Bool -> IO Bool

    • In the expression:
        fun (load @IO @Bool)
          <:
            val (1 :: Double)
              <:
                val (1 :: Int)
                  <: val ("1" :: Text) <: val ('1' :: Char) <: val (1 :: Double)
      In an equation for ‘r’:
          r = fun (load @IO @Bool)
                <:
                  val (1 :: Double)
                    <:
                      val (1 :: Int)
                        <: val ("1" :: Text) <: val ('1' :: Char) <: val (1 :: Double)
    |
143 |     <: val (1 :: Double)
```
It says that we cannot add a function producing an `IO Bool` because that function requires a
`Bool` parameter and nothing produces a `Bool` in the registry.
Unfortunately it also prints out the full expression below the type error.

In some environments, like VS Code, only the top message will appear on top of your code.
In other environments, in GHCi for example, the full error message will be printed out and only
the bottom will be visible. One trick to improve the readibility is to cut to registry right
before the error
```haskell
r :: Registry _ _
r =
     fun (load @IO @Bool)
  <: r2

r2 = val (1 :: Double)
  <: val (1 :: Int)
  <: val ("1" :: Text)
  <: val ('1' :: Char)
  <: val (1 :: Double)
```
Then the error message is limited to
```
  • The function creating the output type

        IO Bool

      cannot be added to the registry because the input parameter

        Bool

       is not one of the registry outputs

      The full function type for IO Bool is

      Bool -> IO Bool

    • In the expression: fun (load @IO @Bool) <: r2
      In an equation for ‘r’: r = fun (load @IO @Bool) <: r2
    |
143 |     <: r2
    |     ^^
```
