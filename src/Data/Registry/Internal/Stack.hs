{-# LANGUAGE ScopedTypeVariables #-}
module Data.Registry.Internal.Stack where

import           Data.Registry.Internal.Operations
import           Data.Registry.Internal.Types
import           Protolude

type Stack a = StateT (Values, Operations) (Either Text) a

runStack :: Stack a -> Values -> Either Text a
runStack sa vs = evalStateT sa (vs, [])

execStack :: Stack a -> Values -> Either Text Values
execStack sa vs = fst <$> execStateT sa (vs, [])

evalStack :: Stack a -> Values -> Either Text Operations
evalStack sa vs = snd <$> execStateT sa (vs, [])

getValues :: Stack Values
getValues = fst <$> get

getOperation :: Stack Operations
getOperation = snd <$> get

modifyValues :: (Values -> Values) -> Stack ()
modifyValues f = modify (\(vs, ops) -> (f vs, ops))

modifyOperation :: (Operations -> Operations) -> Stack ()
modifyOperation f = modify (\(vs, ops) -> (vs, f ops))

dropValues :: (Monad m) => StateT (Values, Operations) m a -> StateT Operations m a
dropValues st = StateT (\s -> (\(a, (_, ops)) -> (a, ops)) <$> runStateT st (mempty, s))

dropOperations :: (Monad m) => StateT (Values, Operations) m a -> StateT Values m a
dropOperations st = StateT (\s -> (\(a, (vs, _)) -> (a, vs)) <$> runStateT st (s, []))

functionApplied :: Value -> [Value] -> Stack ()
functionApplied output inputs = modifyOperation (AppliedFunction output inputs:)
