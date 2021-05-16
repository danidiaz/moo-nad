{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Moo where

import Data.Kind
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader

class HasLogger e d | e -> d where
    logger :: e -> String -> d ()

type M = ReaderT EnvIO IO
type D = IO
type E = EnvIO

data EnvIO = EnvIO {
        _logger :: String -> IO ()
    }
instance HasLogger EnvIO IO where
    logger (EnvIO {_logger}) = _logger

askE :: M E
askE = ask

liftD :: D x -> M x
liftD = lift

