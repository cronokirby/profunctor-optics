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
    , Prism
    , Prism'
    , prism
    , match
    , build
    , Optional
    , Optional'
    , optional
    , preview
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
lens gt st = dimap (\s -> (s, s)) (uncurry st) . first . dimap gt id

-- | Use a lens to view a part of a structure
view :: Lens s t a b -> (s -> a)
view lns = getConstant . runStar (lns (Star Constant))

-- | Use a lens to modify a part of a structure
update :: Optional s t a b -> (b -> s -> t)
update lns = curry $ \(b, s) -> lns (const b) s



-- | Prism's allow accessing parts of a sum
type Prism s t a b = forall p. Choice p => p a b -> p s t

-- | A simplified prism type
type Prism' s a = Prism s s a a

-- | Construct a prism from a matcher, and a builder
prism :: (s -> Either a t) -> (b -> t) -> Prism s t a b
prism matcher builder = dimap matcher (either id id) . left . dimap id builder

-- | Try and match a branch of a sum
match :: Prism s t a b -> (s -> Either a t)
match prsm = runStar (prsm (Star Left))

-- | Build up a structure from one branch
build :: Prism s t a b -> (b -> t)
build prsm = unTagged . prsm . Tagged


-- | A combination of a Lens and a Prism
type Optional s t a b = forall p. (Strong p, Choice p) => p a b -> p s t

-- | A simplified optional type
type Optional' s a = Optional s s a a

-- | Construct a prism from a partial getter, and a setter
optional :: (s -> Either a t) -> (b -> s -> t) -> Optional s t a b
optional p st = dimap preview' merge . left . dimap id (uncurry st) . first
  where
    preview' s = either (\a -> Left (a, s)) Right (p s)
    merge = either id id

-- | Try and access a portion of a structure
preview :: Optional s t a b -> (s -> Either a t)
preview prsm = runStar (prsm (Star Left))
