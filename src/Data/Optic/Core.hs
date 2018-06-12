{-# LANGUAGE RankNTypes #-}
module Data.Optic.Core
    ( Iso
    , Iso'
    , iso
    , from
    , to
    , Lens
    , Lens'
    , lens
    , view
    , update
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



-- | Len's allow accessing and modifying parts of a whole
type Lens s t a b = forall p. Strong p => p a b -> p s t

-- | A simplified Lens that doesn't change types
type Lens' s a = Lens s s a a

-- | Construct a lens from a getter and setter
lens :: (s -> a) -> (b -> s -> t) -> Lens s t a b
lens get set = dimap (\s -> (s, s)) (uncurry set) . first . dimap get id

-- | Use a lens to view a part of a structure
view :: Lens s t a b -> (s -> a)
view lns = getConstant . runStar (lns (Star Constant))

-- | Use a lens to modify a part of a structure
update :: Lens s t a b -> (b -> s -> t)
update lns = curry $ \(b, s) -> lns (const b) s
