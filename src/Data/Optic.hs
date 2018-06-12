{-# LANGUAGE RankNTypes #-}
module Data.Optic
    ((&)
      -- Operators
    , (^.)
    , (.=)
    , over
    , (%=)
      -- Common stuff
    , _1
    , _2
    , module Data.Optic.Core
    )
where

import Data.Function ((&))

import Data.Optic.Core


infixl 8 ^.
-- | View a portion of a structure
-- Equivalent to 'view'
-- >>> (("hello", 1), 2) ^. _1 . _1
-- "hello"
(^.) :: s -> Lens s t a b -> a
s ^. l = view l s


infixr 4 .=
-- | Update a portion of a structure
-- Equivalent to 'update'
(.=) :: Lens s t a b -> b -> s -> t
(.=) = update


-- | Apply a function over a portion of a structure
-- >>> (1, 2) & over _1 (+1)
-- (2, 2)
over :: Lens s t a b -> (a -> b) -> s -> t
over lns f s = update lns (f (view lns s)) s

infixr 4 %=
-- | Synonym for 'over'
(%=) :: Lens s t a b -> (a -> b) -> s -> t
(%=) = over


-- = Common lenses

_1 :: Lens (a, b) (c, b) a c
_1 = lens fst (\c (_, b) -> (c, b))


_2 :: Lens (b, a) (b, c) a c
_2 = lens snd (\c (b, _) -> (b, c))
