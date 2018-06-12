module Data.Profunctor
    ( -- Classes
      Profunctor(..)
    , Strong(..)
      -- Profunctor types
    , Tagged(..)
    , Star(..)
    )
where


-- | A class for Profunctors.
-- This is the weakest in the Profunctor hierarchy.
-- Profunctors can map contravariantly over their first argument,
-- and covariantly over their second.
--
-- ^ dimap id id = id
class Profunctor p where
    -- | Map contravariantly on the front, and covariantly on the back
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d


-- | A class for strong profunctors.
-- Strong profunctors can "run" over one half of a tuple
--
-- ^ dimap (\(a, ()) -> a) (\a -> (a, ()) = first
-- ^ dimap (\(a, (b, c)) -> ((a, b), c))
--         (\((a, b), c) -> (a, (b, c))) (first (first h)) = first h
class Profunctor p => Strong p where
    first :: p a b -> p (a, c) (b, c)


-- == Concrete Profunctor types

-- | A profunctor that ignores its first argument
newtype Tagged a b = Tagged { unTagged :: b }

instance Profunctor Tagged where
    dimap _ f (Tagged b) = Tagged (f b)


-- | Lift a functor over the back of a function
newtype Star f a b = Star { runStar :: a -> f b }

instance Functor f => Profunctor (Star f) where
    dimap ab cd (Star f) = Star (fmap cd . f . ab)


-- | Lift a functor over the front of a function
newtype CoStar f a b = CoStar { runCoStar :: f a -> b }
