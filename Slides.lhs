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
{-# LANGUAGE FlexibleContexts #-}
module Slides where

-- import Data.Functor		-- imported by prelude
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe

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

coordLength :: (Real a, Floating b) => Coord a -> b
coordLength (Coord (x, y)) = sqrt . realToFrac $ x * x + y * y

newtype Extents a = Extents {coordFromExtents :: Coord a} deriving (Eq, Ord)

extentsFromCoord :: Num a => Coord a -> Extents a
extentsFromCoord c = Extents . fmap abs $ c

instance Show a => Show (Extents a) where show = show . coordFromExtents

data Bounds a = Bounds {boundsCentre :: Coord a, boundsExtent :: Extents a}	 deriving (Show, Eq, Ord)

class Divisor a where divideBy' :: a -> a -> a
instance Divisor Double where divideBy' = (/)
instance Divisor Float where divideBy' = (/)
instance Divisor Int where divideBy' = div
instance Divisor Integer where divideBy' = div

instance (Divisor a, Num a, Eq a) => Monoid (Bounds a) where
	-- A zero extents bounds is considered empty
	mempty = Bounds (Coord (0,0)) (extentsFromCoord . Coord $ (0,0))
	-- Appending empty to anything does not change it
	Bounds _ (Extents (Coord (0,0))) `mappend` r = r
	l `mappend` Bounds _ (Extents (Coord (0,0))) = l
	-- Appending two non empties
	l `mappend` r = Bounds c $ extentsFromCoord e
		where
			-- centre is the average of the two centres
			c = (`divideBy'`2) <$> boundsCentre l |+| boundsCentre r
			-- extents is the sum of the two extents
			e = (coordFromExtents . boundsExtent $ l) |+| (coordFromExtents . boundsExtent $ r)
	

type CoordI = Coord Int
type ExtentsI = Extents Int
type BoundsI = Bounds Int

coordI :: Int -> Int -> Coord Int
coordI x y = Coord (x,y)

newtype Fill c a = Fill {runFill :: Coord c -> (Bounds c, a)}

instance Functor (Fill c) where 
	fmap g fl = Fill $ (\(a, b) -> (a, g b)) <$> runFill fl

instance (Monoid a, Monoid (Bounds c)) => Monoid (Fill c a) where
	mempty = Fill . const $ mempty
	a `mappend` b = Fill (runFill a <> runFill b)

fillCircle :: (Real c, Divisor c, Monoid a) => a -> c -> Coord c -> Fill c a
fillCircle val radius pos = Fill produce
	where
	produce crd | coordLength (crd |-| pos) <= realToFrac radius = (bnds, val)
				| otherwise										 = (mempty, mempty)
	bnds = Bounds pos (Extents . Coord $ (radius, radius))

fillRectangle :: (Real c, Divisor c, Monoid a) => a -> c -> c -> Coord c -> Fill c a
fillRectangle val w h pos = Fill produce
	where
	produce crd | let (x, y) = unCoord $ abs <$> (crd |-| pos) 
				  in x <= halfW && y <= halfH 			= (bnds, val)
				| otherwise							    = (mempty, mempty)
	halfW = w `divideBy'` 2
	halfH = h `divideBy'` 2
	bnds = Bounds pos (Extents . Coord $ (halfW, halfH))

class ProduceChar a where produceChar :: a -> Char
instance ProduceChar Char where 
	produceChar = id
instance ProduceChar a => ProduceChar (Maybe a) where
	produceChar Nothing   = ' '
	produceChar (Just a) = produceChar a
instance ProduceChar a => ProduceChar (Last a) where 
	produceChar = produceChar . getLast

drawFillMatrix :: ProduceChar a => Int -> Int -> Fill Int a -> IO ()
drawFillMatrix cs ls fl = ios cs ls
	where
		flToChar = runFill . fmap produceChar $ fl
		ios 0 0 = putChar '\n'
		ios 0 l = putChar '\n' >> ios cs (l-1)
		ios c l = (putChar . snd . flToChar $ Coord (c,l)) >> ios (c-1) l

myPicture :: IO ()
myPicture = drawFillMatrix 20 20 
			(  fillRectangle (Last . Just $ '@') 20 20 (coordI 10 10)
			<> fillCircle (Last . Just $ '*') 5 (coordI 10 10)
			<> fillCircle (Last . Just $ '#') 5 (coordI 12 12)
			<> fillRectangle (Last . Just $ '$') 5 8 (coordI 15 15)
			)

main :: IO ()
main = do
	putStrLn "myPicture"
	myPicture
	putStrLn "myDone"
	print . coordLength . Coord $ (0,0)
	putStrLn "done"
\end{code}
