{-# LANGUAGE RankNTypes #-}
module Data.Optic
    ((&)
      -- Operators
    , (^.)
    , (.=)
    , over
    , (%=)
    , (^?)
      -- Common stuff
    , _1
    , _2
    , the
    , _Left
    , _Right
    , ix
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
(.=) :: Optional s t a b -> b -> s -> t
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


infixl 8 ^?
-- | Try and view part of a structure
(^?) :: s -> Optional s t a b -> Maybe a
s ^? prsm = either Just (const Nothing) (preview prsm s)



-- = Common Optics

_1 :: Lens (a, b) (c, b) a c
_1 = lens fst (\c (_, b) -> (c, b))

_2 :: Lens (b, a) (b, c) a c
_2 = lens snd (\c (b, _) -> (b, c))


the :: Prism (Maybe a) (Maybe b) a b
the = prism (maybe (Right Nothing) Left) Just


_Left :: Prism (Either a c) (Either b c) a b
_Left = prism (fmap Right) Left

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism (either (Right . Left) Left) Right


ix :: Int -> Optional [a] [a] a a
ix i = optional (\l -> maybe (Right l) Left $ safeGet i l) (set i)
  where
    safeGet _ []     = Nothing
    safeGet 0 (x:xs) = Just x
    safeGet i (x:xs) = safeGet (i - 1) xs
    set _ _ []       = []
    set 0 x (_:xs)   = x:xs
    set i s (x:xs)   = x : set (i - 1) s xs
