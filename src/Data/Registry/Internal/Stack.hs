{-# LANGUAGE ScopedTypeVariables #-}
module Data.Registry.Internal.Stack where

import           Control.Monad.Trans.Writer.Lazy
import           Data.Registry.Internal.Types
import           Protolude

type Stack a = WriterT () (StateT Values (Either Text)) a

runStack :: Stack a -> Values -> Either Text a
runStack = evalStateT . dropWriterT

dropWriterT :: (Functor m) => WriterT w m a -> m a
dropWriterT ma = fst <$> runWriterT ma
