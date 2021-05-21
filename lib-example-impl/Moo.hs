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

class HasLogger d e | e -> d where
    logger :: e -> String -> d ()

newtype Logger m = Logger { runLogger :: Int -> String -> m () }

type D = IO
liftD :: D x -> M x
liftD = lift

data EnvIO = EnvIO {
        _logger :: String -> IO (),
        _loggerComponent :: Logger IO
    }

instance HasLogger D E where
    logger (EnvIO {_logger}) = _logger

instance Has Logger D E where
    dep (EnvIO {_loggerComponent}) = _loggerComponent

