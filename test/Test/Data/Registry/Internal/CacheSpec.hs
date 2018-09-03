module Test.Data.Registry.Internal.CacheSpec where

{-

  Internal Test plan

   - Cache: create cache, put and get values concurrently
   - Dynamic: isFun with various types of functions including with constraints
       applyFun with various types of functions

   - Make: regular makeUnsafe, cycle makeUnsafe

   - Reflection: different cases of showing values and `m a`

   - Registry:
      findValue
      findConstructor
      storeValue with modifiers

  API

    example with gens
    specialize
    singleton

    RIO

    resources allocation
    warmup messages







-}