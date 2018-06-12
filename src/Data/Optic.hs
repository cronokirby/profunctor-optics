{-# LANGUAGE RankNTypes #-}
module Data.Optic
    (
      Iso
    , Iso'
    , iso
    , from
    , to
    )
where

import Data.Profunctor


{- Concrete types we need to implement the lens functions -}

newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a


-- | Iso's allow converting from and to types
type Iso s t a b = forall p. Profunctor p => p a b -> p s t

-- | A simplified Iso that doesn't change types
type Iso' a b = Iso a a b b

-- | Construct an isomorphism from two functions
-- The functions should complement eachother:
-- ^ iso f g => f . g = id
iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso = dimap


from :: Iso s t a b -> (s -> a)
from is = getConstant . runStar (is (Star Constant))

to :: Iso s t a b -> (b -> t)
to is = unTagged . is . Tagged
