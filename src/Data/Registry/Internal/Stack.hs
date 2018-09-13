{-# LANGUAGE ScopedTypeVariables #-}
module Data.Registry.Internal.Stack where

import           Data.Registry.Internal.Operations
import           Data.Registry.Internal.Types
import           Protolude

type Stack a = StateT (Values, Operation) (Either Text) a

runStack :: Stack a -> Values -> Either Text a
runStack = evalStateT . dropOperations

execStack :: Stack a -> Values -> Either Text Values
execStack = execStateT . dropOperations

getValues :: Stack Values
getValues = fst <$> get

getOperation :: Stack Operation
getOperation = snd <$> get

modifyValues :: (Values -> Values) -> Stack ()
modifyValues f = modify (\(vs, ops) -> (f vs, ops))

modifyOperation :: (Operation -> Operation) -> Stack ()
modifyOperation f = modify (\(vs, ops) -> (vs, f ops))

dropValues :: (Monad m) => StateT (Values, Operation) m a -> StateT Operation m a
dropValues st = StateT (\s -> (\(a, (_, ops)) -> (a, ops)) <$> runStateT st (mempty, s))

dropOperations :: (Monad m) => StateT (Values, Operation) m a -> StateT Values m a
dropOperations st = StateT (\s -> (\(a, (vs, _)) -> (a, vs)) <$> runStateT st (s, Empty))

functionApplied :: Value -> Stack ()
functionApplied = modifyOperation . AppliedFunction

foundValue :: Value -> Stack ()
foundValue = modifyOperation . RetrievedValue
