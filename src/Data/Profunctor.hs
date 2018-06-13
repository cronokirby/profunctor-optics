module Data.Profunctor
    ( -- Classes
      Profunctor(..)
    , Strong(..)
    , Choice(..)
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


-- | The dual to strong profunctors.
-- Instead of running over one side of a product,
-- they run over one side of a sum.
class Profunctor p => Choice p where
    left :: p a b -> p (Either a c) (Either b c)



-- == Concrete Profunctor types

-- function instances
instance Profunctor (->) where
    dimap f h g = h . g . f

instance Strong (->) where
    first f = \(a, c) -> (f a, c)


-- | A profunctor that ignores its first argument
newtype Tagged a b = Tagged { unTagged :: b }

instance Profunctor Tagged where
    dimap _ f (Tagged b) = Tagged (f b)

instance Choice Tagged where
    left (Tagged b) = Tagged (Left b)

-- | Lift a functor over the back of a function
newtype Star f a b = Star { runStar :: a -> f b }

instance Functor f => Profunctor (Star f) where
    dimap ab cd (Star f) = Star (fmap cd . f . ab)

instance Functor f => Strong (Star f) where
    first (Star f) = Star (\(a, c) -> (flip (,) c) <$> f a)

instance Applicative f => Choice (Star f) where
     left (Star f) = Star (either (fmap Left . f) (pure . Right))


-- | Lift a functor over the front of a function
newtype CoStar f a b = CoStar { runCoStar :: f a -> b }
