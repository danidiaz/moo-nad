{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Invocation helpers for functions that have effects in a monad 'D' and are carried
-- in the environment 'E' of a reader-like monad 'M'.
module Moo.Prelude (
    self,
    call,
    -- * Re-exports from Moo
    M,
    E,
    D 
) where

import Moo
import Data.Kind
import GHC.TypeLits
import Control.Monad.Reader
import Control.Monad.Dep.Has

type Call :: Type -> Constraint
class Call curried where
    type LiftedD curried :: Type
    -- | Given a way of extracting from the environment 'E' a @curried@
    -- function that ends in a 'D' action, lift the @curried@ 'D'-function into the main
    -- monad 'M'.
    self :: (E -> curried) -> LiftedD curried

instance Call (D r) where
    type LiftedD (D r) = M r
    self extractor = do
        e <- ask
        liftD $ extractor e

instance Call curried' => Call (a -> curried') where
    type LiftedD (a -> curried') = a -> LiftedD curried'
    self extractor a = 
        let extractor' = \e -> extractor e a
        in self @curried' extractor'

-- | Provided that the environment 'E' 'Control.Monad.Dep.Has' a @component@, and
-- given a way of extracting from the @component@ a @curried@ function that
-- ends in a 'D' action, lift the @curried@ 'D'-function into the main monad
-- 'M'.
--
-- The extractor must be monomorphic on the @component@, so that the intended
-- instance of 'Control.Monad.Dep.Has' is found. 
--
-- The typical case is for the @component@ to be a parameterized record and for
-- the extractor to be a field accessor.
call :: forall component curried . (Has component D E, Call curried) => (component D -> curried) -> LiftedD curried
call extractor = self (extractor . dep @component)

