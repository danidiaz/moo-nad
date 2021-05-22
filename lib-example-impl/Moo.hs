{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | This is the implementation module for the signature of the same name.
module Moo (module Moo, runReaderT) where

import Data.Kind
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Dep.Has

type M = ReaderT EnvIO IO
type E = EnvIO
type D = IO
liftD :: D x -> M x
liftD = lift

-- | Same definition as the one in the module signature.
class HasLogger d e | e -> d where
    logger :: e -> Int -> String -> d ()

-- | Same definition as the one in the module signature.
data Counter d = Counter { 
        askCounter :: d Int,
        incCounter :: Int -> d ()
    }

-- | This is the point at which we define a concrete type for the environment.
--
-- The 'HasLogger' instance is backed by a bare function defined at the top
-- level, while the 'Counter' component is a field by itself.
data EnvIO = EnvIO {
        _logger :: Int -> String -> IO (),
        _counter :: Counter IO
    }

instance HasLogger D E where
    logger (EnvIO {_logger}) = _logger

-- | 'Control.Monad.Dep.Has' establishes a relationship between the environment 'E',
-- the nominal record defining a component, and the effect monad the component
-- uses in the environment..
instance Has Counter D E where
    dep (EnvIO {_counter}) = _counter

