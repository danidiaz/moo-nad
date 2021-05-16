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

module Moo.Prelude (module Moo) where

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
    call extractor a = _

-- class CurryliftD d m where
--   multiliftD :: d -> m
-- 
-- instance CurryliftD (D x) (M x) where
--   multiliftD = liftD
-- 
-- instance CurryliftD d m => MultiliftD (a -> d) (a -> m) where
--   multiliftD f a = multiliftD @d @m (f a)
-- 
-- class CurryjoinD curried where
-- 
-- call :: (E -> curryable) -> curriedTo 

someFunc :: IO ()
someFunc = putStrLn "someFunc"
