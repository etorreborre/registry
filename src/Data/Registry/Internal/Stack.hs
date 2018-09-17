{- |
  Internal monad for the resolution algorithm

   - we keep some state for the list of created values
   - we collect the applied functions as "Operations"
   - we might exit with a Left value if we can't build a value

-}
module Data.Registry.Internal.Stack where

import           Data.Registry.Internal.Operations
import           Data.Registry.Internal.Types
import           Protolude

-- | Monadic stack for the resolution algorithm
type Stack a = StateT (Values, Operations) (Either Text) a

-- | Return a value from the Stack if possible
runStack :: Stack a -> Values -> Either Text a
runStack sa vs = evalStateT sa (vs, [])

-- | Return the state of the stack after executing the action
--   This returns the list of built values
execStack :: Stack a -> Values -> Either Text Values
execStack sa vs = fst <$> execStateT sa (vs, [])

-- | Return the list of applied functions after resolution
evalStack :: Stack a -> Values -> Either Text Operations
evalStack sa vs = snd <$> execStateT sa (vs, [])

-- | Get the current list of values
getValues :: Stack Values
getValues = fst <$> get

-- | Get the current list of operations
getOperation :: Stack Operations
getOperation = snd <$> get

-- | Modify the current list of values
modifyValues :: (Values -> Values) -> Stack ()
modifyValues f = modify (\(vs, ops) -> (f vs, ops))

-- | Get the current list of values
modifyOperations :: (Operations -> Operations) -> Stack ()
modifyOperations f = modify (\(vs, ops) -> (vs, f ops))

-- | Store a function application in the list of operations
functionApplied :: Value -> [Value] -> Stack ()
functionApplied output inputs = modifyOperations (AppliedFunction output inputs:)
