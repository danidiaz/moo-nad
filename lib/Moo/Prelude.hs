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

module Moo.Prelude (M, E, D, call) where

import Moo
import Data.Kind
import GHC.TypeLits

type Call :: Type -> Constraint
class Call curried where
    type LiftedD curried :: Type
    call :: (E -> curried) -> LiftedD curried

instance Call (D r) where
    type LiftedD (D r) = M r
    call extractor = do
        e <- askE
        liftD $ extractor e

instance Call curried' => Call (a -> curried') where
    type LiftedD (a -> curried') = a -> LiftedD curried'
    call extractor a = 
        let extractor' = \e -> extractor e a
        in call @curried' extractor'

