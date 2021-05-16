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

type Multicurryable :: [Type] -> Type -> Type -> Constraint
class Multicurryable as r curried | curried -> as r where
    type LiftedD as r curried :: Type
    call :: (E -> curried) -> LiftedD as r curried

instance Multicurryable '[] r (D r) where
    type LiftedD '[] r (D r) = M r
    call extractor = do
        e <- askE
        liftD $ extractor e

instance Multicurryable as r curried' => Multicurryable (a ': as) r (a -> curried') where
    type LiftedD (a ': as) r (a -> curried') = a -> LiftedD as r curried'
    call extractor a = 
        let extractor' = \e -> extractor e a
        in call @as @r @curried' extractor'

