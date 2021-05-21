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

module Moo.Prelude (M, E, D, self, call) where

import Moo
import Data.Kind
import GHC.TypeLits
import Control.Monad.Reader
import Control.Monad.Dep.Has

type Call :: Type -> Constraint
class Call curried where
    type LiftedD curried :: Type
    -- | Given a way of extracting from the environment 'E' a @curried@
    -- function that ends in a 'D' action, lift the 'D'-function into the main
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

-- | Given an environment 'E' that 'Control.Monad.Dep.Has' a @component@, and a way of extracting
-- from the @component@ a @curried@ function that ends in a 'D' action, lift the
-- 'D'-function into the main monad 'M'.
call :: forall component curried . (Has component D E, Call curried) => (component D -> curried) -> LiftedD curried
call extractor = self (extractor . dep @component)

