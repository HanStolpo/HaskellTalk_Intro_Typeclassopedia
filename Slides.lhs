% Haskell Basics and Typeclassopedia
% Handré Stolp
% January 13, 2014

Introduction
==========

What will be covered
-----------
* Basic syntax of Haskell
* First few type classes from the Typeclassopedia

How is it presented
-----------------
* As literate Haskell
* Can press `A` and copy all the text to a `.lhs` file and run in GHCi


Begin file
==========
here is the start of the file

\begin{code}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
module Slides where

-- import Data.Functor		-- imported by prelude
import Control.Applicative
import Data.Monoid
\end{code}

Functor
===========

````haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
````

Applicative
=============

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Main
==========
Here is the main function

\begin{code}

newtype Coord a = Coord {unCoord :: (a,a)} deriving (Eq, Ord)

instance Show a => Show (Coord a) where show = show . unCoord

instance Functor Coord  where
	fmap f (Coord (x,y)) = Coord (f x, f y)

instance Applicative Coord where
	pure a = Coord (a,a)
	Coord (g, h) <*> Coord (x, y) = Coord (g x, h y)

class Additive a where
	(|+|) :: a -> a -> a
	infixl 6 |+|
	(|-|) :: a -> a -> a
	infixl 6 |-|

instance Num a => Additive (Coord a) where
	a |+| b = (+) <$> a <*> b
	a |-| b = (-) <$> a <*> b

-- type family Element a
-- type instance Element (Coord a) = a
-- 
-- class Scalable a where
-- 	(|/) :: a -> Element a -> a
-- 	infixl 7 |/
-- 	(|*) :: a -> Element a -> a
-- 	infixl 7 |*
-- 	(*|) ::  Element a -> a -> a
-- 	infixl 7 *|
-- 
-- instance Floating a => Scalable (Coord a) where
-- 	c |/ s = (/s) <$> c
-- 	c |* s = (*s) <$> c
-- 	s *| c = (s*) <$> c
-- 
-- instance Scalable (Coord Int) where
-- 	c |/ s = (`div`s) <$> c
-- 	c |* s = (*s) <$> c
-- 	s *| c = (s*) <$> c

coordLength :: (Real a, Num a, Floating b) => Coord a -> b
coordLength (Coord (x, y)) = sqrt . realToFrac $ x * x + y * y



newtype Extents a = Extents {coordFromExtents :: Coord a} deriving (Eq, Ord)

extentsFromCoord :: Num a => Coord a -> Extents a
extentsFromCoord c = Extents . fmap abs $ c

instance Show a => Show (Extents a) where show = show . coordFromExtents

data Bounds a = Bounds {boundsCentre :: Coord a, boundsExtent :: Extents a}	 deriving (Show, Eq, Ord)

instance Integral a => Monoid (Bounds a) where
	mempty = Bounds (Coord (0,0)) (extentsFromCoord . Coord $ (0,0))
	Bounds _ (Extents (Coord (0,0))) `mappend` r = r
	l `mappend` Bounds _ (Extents (Coord (0,0))) = l
	l `mappend` r = Bounds c $ extentsFromCoord e
		where
			c = (`div`2) <$> boundsCentre l |+| boundsCentre r
			e = (coordFromExtents . boundsExtent $ l) |+| (coordFromExtents . boundsExtent $ r)
	

type CoordI = Coord Int
type ExtentsI = Extents Int
type BoundsI = Bounds Int

-- newtype Fill a = Fill {runFill :: CoordI -> (Bounds, a)}

main :: IO ()
main = do
	print . coordLength . Coord $ (0,0)
	putStrLn "done"
\end{code}
