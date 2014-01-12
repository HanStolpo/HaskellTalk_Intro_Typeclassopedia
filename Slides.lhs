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
{-# LANGUAGE FlexibleContexts, BangPatterns #-}
module Slides where

-- import Data.Functor      -- imported by prelude
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import Debug.Trace

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

instance Monoid a => Monoid (Coord a) where
   mempty = Coord (mempty, mempty)
   Coord (lx, ly) `mappend` Coord (rx, ry) = Coord (lx <> rx, ly <> ry)


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

data Bounds a = Bounds { boundsCentre :: Coord a
                       , boundsExtent :: Extents a
                       }  deriving (Show, Eq, Ord)

pointInBounds :: (Num a, Ord a, Show a) => Bounds a -> Coord a -> Maybe (Coord a)
pointInBounds b c = let
        cntr = boundsCentre b
        Coord (ex, ey) = coordFromExtents . boundsExtent $ b
        Coord (dx, dy) = c |-| cntr
        ox = if dx < 0 then ex + dx else ex - dx
        oy = if dy < 0 then ey + dy else ey - dy
        in 
            if (abs dx < ex && abs dy < ey) 
            then Just . Coord $ (ox, oy)
            else Nothing 


class Divisor a where divideBy' :: a -> a -> a
instance Divisor Double where divideBy' = (/)
instance Divisor Float where divideBy' = (/)
instance Divisor Int where divideBy' = div
instance Divisor Integer where divideBy' = div

instance (Divisor a, Num a, Eq a, Show a) => Monoid (Bounds a) where
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

instance (Monoid a, Monoid (Bounds c), Show c) => Monoid (Fill c a) where
    mempty = Fill . const $ mempty
    a `mappend` b = Fill (runFill a <> runFill b)

moveFill :: Num c => Coord c -> Fill c a -> Fill c a 
moveFill c = Fill . (. (|-| c)) . runFill

fillCircle :: (Real c, Divisor c, Monoid a, Show c) => a -> c -> Coord c -> Fill c a
fillCircle val radius pos = Fill produce
    where
    produce crd | coordLength (crd |-| pos) <= realToFrac radius  = (bnds, val)
                | otherwise                                       = (bnds, mempty)
    bnds = Bounds pos (Extents . Coord $ (radius, radius))

fillRectangle :: (Real c, Divisor c, Monoid a, Show c) => a -> c -> c -> Coord c -> Fill c a
fillRectangle val w h pos = Fill produce
    where
    produce crd | let (x, y) = unCoord $ abs <$> (crd |-| pos) 
                  in x <= halfW && y <= halfH           = (bnds, val)
                | otherwise                             = (bnds, mempty)
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
drawFillMatrix cs ls fl = putStrLn $ ios cs2 ls
    where
        cs2      = cs * 2
        flToChar = runFill . fmap produceChar $ fl
        ios 0 0  = []
        ios 0 l  = '\n' : ios cs2 (l-1)
        ios c l  = (snd . flToChar $ Coord (cs - c `div` 2, ls - l)) : ios (c-1) l

lastChar :: Char -> Last Char
lastChar = Last . Just


myPicture :: IO ()
myPicture = drawFillMatrix 40 40 
            (
               fillCircle    (lastChar 'X') 11  (coordI 15 15)
            <> fillCircle    (lastChar '#') 7   (coordI 15 15)
            <> fillRectangle (lastChar '$') 6 6 (coordI 15 15)
            <> fillCircle    (lastChar ' ') 2   (coordI 15 15)

            <> fillRectangle (lastChar '+') 1 1 (coordI 0 0)
            <> fillRectangle (lastChar '+') 1 1 (coordI 40 40)
            <> fillRectangle (lastChar '+') 1 1 (coordI 40 0)
            <> fillRectangle (lastChar '+') 1 1 (coordI 0 40)
            )

data ShipType = Cruiser | Destroyer
instance ProduceChar ShipType where
    produceChar Cruiser = 'C'
    produceChar Destroyer = 'D'

data ShipOrientation = ShipVertical | ShipHorizontal deriving (Show, Eq, Ord)
data GameState = GameState -- {} deriving (Show, Eq, Ord)

cruiser :: ShipOrientation -> CoordI -> Fill Int (Last ShipType)
cruiser o pos = case o of
    ShipVertical    -> fillRectangle t 2 3 pos <> fillCircle t 2 (pos |-| coordI 0 3)
    ShipHorizontal  -> fillRectangle t 3 2 pos <> fillCircle t 2 (pos |-| coordI 3 0)
    where
        t = Last . Just $ Cruiser

destroyer :: ShipOrientation -> CoordI -> Fill Int (Last ShipType)
destroyer o pos = case o of
    ShipVertical    -> fillRectangle t 1 2 pos <> fillCircle t 2 (pos |-| coordI 0 3) <> fillCircle t 2 (pos |+| coordI 0 3)
    ShipHorizontal  -> fillRectangle t 2 1 pos <> fillCircle t 2 (pos |-| coordI 3 0) <> fillCircle t 2 (pos |+| coordI 3 0)
    where
        t = Last . Just $ Destroyer

myGameBoard :: IO ()
myGameBoard = drawFillMatrix 40 40 (  cruiser ShipHorizontal (coordI 8 8) 
                                   <> cruiser ShipVertical (coordI 14 14)
                                   <> destroyer ShipVertical (coordI 28 28)
                                   <> destroyer ShipHorizontal (coordI 34 34)
                                   )


newtype Max a = Max {unMax :: Maybe a}
instance Ord a => Monoid (Max a) where
   mempty = Max Nothing
   Max Nothing `mappend` r = r
   l `mappend` Max Nothing  = l
   Max (Just l) `mappend` Max (Just r)  = Max . Just . max l $ r

layoutBoard :: Int -> Int -> [Fill Int (Last ShipType)] -> [Fill Int (Last ShipType)]
layoutBoard _ _ [] = []
layoutBoard w h ships = ships'
    where
        ships' = foldl findPlace [] shipsInBnds
        -- ships' = shipsInBnds
        shipsInBnds = map toBnds ships
        corners = [coordI x y | x <- [0,w], y <- [0,h]]
        toBnds s = let 
                    (bnds, _) = runFill s (coordI 0 0)
                    Coord (cx, cy) = boundsCentre bnds
                    in case (fmap . fmap $ unMax) 
                             .  mconcat 
                             .  map (fmap . fmap $ Max . Just) 
                             .  map (pointInBounds bnds) 
                             $ corners of
                        Nothing -> s
                        Just (Coord (Just dx, Just dy))  -> let
                                    ox = if cx >= w `div` 2 then dx else -dx
                                    oy = if cy >= h `div` 2 then dy else -dy
                                    in moveFill (coordI ox oy) s
        findPlace [] n = [n]
        findPlace ps n = ps <> [offset (mconcat ps) n (coordI 0 0) 0]
        offset chk n o m = 
                         let 
                         n' = if m >= 2 * w * h then error "fails" else moveFill o n
                         (bnds, _) = runFill n' (coordI 0 0)
                         Coord (cx, cy) = boundsCentre bnds
                         Coord (ex, ey) = coordFromExtents . boundsExtent $ bnds
                         cs = [coordI x y | x <- [(cx - ex) .. (cx + ex)], y <- [(cy - ey) .. (cy + ey)]]
                         in if noCol chk n' cs 
                            then n'
                            else offset chk n (incOff o) (m+1)
        noCol chk n cs =  not . getAny . mconcat . map (Any . col chk n) $ cs
        col chk n c = let 
                         a = getLast . snd . runFill chk $ c
                         b = getLast . snd . runFill n $ c
                         bndsa = fst . runFill chk $ c
                         bndsb = fst . runFill n $ c
                         in case (a,b) of
                            (Just _, Just _) -> True
                            _ -> False
        inBnds bnds =  not . isJust . getLast . mconcat . map (Last . pointInBounds bnds) $ corners
        incOff (Coord (x,y)) | x >= w && y >= h = Coord (0, 0)
                             | x >= w           = Coord (0, y + 1)
                             | otherwise        = Coord (x + 1, y)

pickShips :: [Fill Int (Last ShipType)]
pickShips = getZipList  
            $   ZipList (replicate 1 destroyer <> replicate 2 cruiser) 
            <*> ZipList (cycle [ShipVertical, ShipVertical, ShipVertical])
            -- <*> ZipList (cycle [coordI 0 0])
             <*> ZipList (getZipList $ ZipList (cycle [coordI]) <*> ZipList [(x * 0 + 20)| x <- [0..5]] <*> ZipList [(y * 0 + 20)| y <- [0..5]])

myGameBoard2 :: IO ()
myGameBoard2 = drawFillMatrix 40 40 . mconcat $ (layoutBoard 40 40 pickShips)

-- myGameBoard2 :: IO ()
-- myGameBoard2 = drawFillMatrix 40 40 . mconcat $ (layoutBoard 40 40 [ destroyer ShipVertical (coordI 40 40)
--                                                                      , destroyer ShipVertical (coordI 0 0)
--                                                                      , destroyer ShipVertical (coordI 40 0)])
--
main :: IO ()
main = do
    -- putStrLn "myPicture"
    -- myPicture
    -- putStrLn "myDone"
    -- myGameBoard
    myGameBoard2
    print . coordLength . Coord $ (0,0)
    putStrLn "done"
\end{code}
