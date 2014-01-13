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
import Data.Char
import Debug.Trace
import System.Random

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
        -- ox = if dx < 0 then ex + dx else ex - dx
        -- oy = if dy < 0 then ey + dy else ey - dy
        msg = "b = " ++ show b ++ " c = " ++ show c ++ " o = " ++ show (dx, dy)
        in 
            if (abs dx < ex && abs dy < ey) 
            then trace ("Just " ++ msg) Just . Coord $ (dx, dy)
            else trace ("Nothing " ++ msg) Nothing 


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

data Fill c a = Fill  { queryFill  :: Coord c -> a
                      , fillBounds :: Bounds c
                      , moveFill   :: Coord c -> Fill c a
                      }

instance Functor (Fill c) where
    fmap g Fill  { queryFill = q
                 , fillBounds = b
                 , moveFill = m
                 } = Fill (fmap g q) b ((fmap . fmap) g m)

instance (Monoid a, Monoid (Bounds c), Show c) => Monoid (Fill c a) where
    mempty = Fill (const mempty) mempty (const mempty)
    a `mappend` b = Fill (queryFill a <> queryFill b) (fillBounds a <> fillBounds b) (moveFill a <> moveFill b)



fillCircle :: (Real c, Divisor c, Monoid a, Show c) => a -> c -> Coord c -> Fill c a
fillCircle val radius pos = Fill qry bnds mv
    where
    qry crd | coordLength (crd |-| pos) <= realToFrac radius  = val
            | otherwise                                       = mempty
    bnds = Bounds pos (Extents . Coord $ (radius, radius))
    mv = fillCircle val radius . (pos |+|) 

fillRectangle :: (Real c, Divisor c, Monoid a, Show c) => a -> c -> c -> Coord c -> Fill c a
fillRectangle val w h pos = Fill qry bnds mv
    where
    qry crd | let (x, y) = unCoord $ abs <$> (crd |-| pos) 
                  in x <= halfW && y <= halfH       = val
            | otherwise                             = mempty
    halfW = w `divideBy'` 2
    halfH = h `divideBy'` 2
    bnds = Bounds pos (Extents . Coord $ (halfW, halfH))
    mv = fillRectangle val w h . (pos |+|) 



-- newtype Fill c a = Fill {runFill :: Coord c -> (Bounds c, a)}
-- 
-- instance Functor (Fill c) where 
--     fmap g fl = Fill $ (\(a, b) -> (a, g b)) <$> runFill fl
-- 
-- instance (Monoid a, Monoid (Bounds c), Show c) => Monoid (Fill c a) where
--     mempty = Fill . const $ mempty
--     a `mappend` b = Fill (runFill a <> runFill b)
-- 
-- moveFill :: Num c => Coord c -> Fill c a -> Fill c a 
-- moveFill c = Fill . (. (|-| c)) . runFill
-- 
-- fillCircle :: (Real c, Divisor c, Monoid a, Show c) => a -> c -> Coord c -> Fill c a
-- fillCircle val radius pos = Fill produce
--     where
--     produce crd | coordLength (crd |-| pos) <= realToFrac radius  = (bnds, val)
--                 | otherwise                                       = (bnds, mempty)
--     bnds = Bounds pos (Extents . Coord $ (radius, radius))
-- 
-- fillRectangle :: (Real c, Divisor c, Monoid a, Show c) => a -> c -> c -> Coord c -> Fill c a
-- fillRectangle val w h pos = Fill produce
--     where
--     produce crd | let (x, y) = unCoord $ abs <$> (crd |-| pos) 
--                   in x <= halfW && y <= halfH           = (bnds, val)
--                 | otherwise                             = (bnds, mempty)
--     halfW = w `divideBy'` 2
--     halfH = h `divideBy'` 2
--     bnds = Bounds pos (Extents . Coord $ (halfW, halfH))

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
        flToChar = queryFill . fmap produceChar $ fl
        ios 0 0  = []
        ios 0 l  = '\n' : ios cs2 (l-1)
        ios c l  = (flToChar $ Coord (cs - c `div` 2, ls - l)) : ios (c-1) l

lastChar :: Char -> Last Char
lastChar = Last . Just


myPicture :: IO ()
myPicture = drawFillMatrix 40 40 (border <> moveFill image (coordI 5 5))
    where
        border =  fillRectangle (lastChar '+') 1 1 (coordI 0 0)
               <> fillRectangle (lastChar '+') 1 1 (coordI 40 40)
               <> fillRectangle (lastChar '+') 1 1 (coordI 40 0)
               <> fillRectangle (lastChar '+') 1 1 (coordI 0 40)

        image =  fillCircle    (lastChar 'X') 11  (coordI 15 15)
              <> fillCircle    (lastChar '#') 7   (coordI 15 15)
              <> fillRectangle (lastChar '$') 6 6 (coordI 15 15)
              <> fillCircle    (lastChar ' ') 2   (coordI 15 15)


data ShipType = Cruiser | Destroyer
instance ProduceChar ShipType where
    produceChar Cruiser = 'C'
    produceChar Destroyer = 'D'

data ShipOrientation = ShipVertical | ShipHorizontal deriving (Show, Eq, Ord, Bounded)
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



layoutBoard :: Int -> Int -> [Fill Int (Last ShipType)] -> [Fill Int (Last ShipType)]
layoutBoard _ _ [] = []
layoutBoard w h ships = ships'
    where
        ships' = foldl findPlace [] shipsInBnds
        -- ships' = shipsInBnds
        -- ships' = ships
        shipsInBnds = map toBnds ships

        toBnds s = let 
                    bnds  = fillBounds s 
                    Coord (cx, cy) = boundsCentre bnds
                    Coord (ex, ey) = coordFromExtents . boundsExtent $ bnds
                    dx = if cx < ex then ex - cx else (if cx + ex > w then w - cx - ex  else 0)
                    dy = if cy < ey then ey - cy else (if cy + ey > h then h - cy - ey  else 0)
                    in moveFill s (coordI dx dy)

        findPlace [] n = [n]
        findPlace ps n = ps <> [offset (mconcat ps) n (coordI 0 0) 0]

        offset chk n o m = 
                         let 
                         n' = if m >= 2 * w * h then error "fails" else moveFill n o 
                         bnds = fillBounds n'
                         Coord (cx, cy) = boundsCentre bnds
                         Coord (ex, ey) = coordFromExtents . boundsExtent $ bnds
                         cs = [coordI x y | x <- [(cx - ex) .. (cx + ex)], y <- [(cy - ey) .. (cy + ey)]]
                         in if isOk chk n' cs 
                            then n'
                            else offset chk n (incOff o) (m+1)

        isOk chk n cs = let 
                        bnds  = fillBounds n
                        Coord (cx, cy) = boundsCentre bnds
                        Coord (ex, ey) = coordFromExtents . boundsExtent $ bnds
                        xOk = cx >= ex && cx + ex <= w
                        yOk = cy >= ey && cy + ey <= h
                        in xOk && yOk && (not . getAny . mconcat . map (Any . col chk n) $ cs)

        col chk n c = let 
                         a = getLast . queryFill chk $ c
                         b = getLast . queryFill n $ c
                         in case (a,b) of
                            (Just _, Just _) -> True
                            _                -> False

        incOff (Coord (x,y)) | x >= w && y >= h = Coord (0, 0)
                             | x >= w           = Coord (0, y + 1)
                             | otherwise        = Coord (x + 1, y)

pickShips :: [Fill Int (Last ShipType)]
pickShips = getZipList  
            $   ZipList (replicate 3 destroyer <> replicate 2 cruiser) 
            <*> ZipList (cycle [ShipVertical, ShipHorizontal])
            <*> ZipList (cycle [coordI 0 0])
            -- <*> ZipList (getZipList $ ZipList (cycle [coordI]) <*> ZipList [(x * 0 + 20)| x <- [0..5]] <*> ZipList [(y * 0 + 20)| y <- [0..5]])

myGameBoard2 :: IO ()
myGameBoard2 = drawFillMatrix 40 40 . mconcat $ (layoutBoard 40 40 pickShips)

-- myGameBoard2 :: IO ()
-- myGameBoard2 = drawFillMatrix 40 40 . mconcat $ (layoutBoard 40 40 [ destroyer ShipVertical (coordI 40 40)
--                                                                      , destroyer ShipVertical (coordI 0 0)
--                                                                      , destroyer ShipVertical (coordI 40 0)])
--

randomBoard :: StdGen -> (StdGen, [Fill Int (Last ShipType)])
randomBoard gen = 
    let ship = ZipList . map (\a -> if a == 0 then destroyer else cruiser) . randomRs (0 :: Int, 1 :: Int) $ gen
        orient = ZipList . map (\a -> if a == 0 then ShipVertical else ShipHorizontal) . randomRs (0 :: Int, 1 :: Int) $ gen
        cxs = ZipList . randomRs (0, 40) $ gen
        cys = ZipList . randomRs (0, 40) $ gen
    in (mkStdGen . fst . random $ gen, layoutBoard 40 40 . take 5 . getZipList $ ship <*> orient <*> (coordI <$> cxs <*> cys))
       
data Game = Game { ships     :: [Fill Int (Last ShipType)]
                 , board     :: Fill Int (Last Char)
                 }

shipToBrd :: Fill Int (Last ShipType) -> Fill Int (Last Char)
shipToBrd s = Last . (fmap produceChar) . getLast <$> s

drawBrd :: Fill Int (Last Char) -> IO ()
drawBrd = drawFillMatrix 40 40  

playNewGame :: StdGen -> IO ()
playNewGame gen = let 
                  (gen', ships) = randomBoard gen
                  border = mconcat $ map (fillRectangle (lastChar '+') 1 1) (coordI <$> [x | x <- [0,40]] <*> [y | y <- [0,40]]) 
                  in playGame gen' (Game ships border)

wonGame :: StdGen -> IO () 
wonGame gen = do
    putStrLn "You won the game. Play another ? 'y'/'n'"
    t <- getLine
    case filter (not . isSpace) t of
        'y' : _ -> playNewGame gen
        'Y' : _ -> playNewGame gen
        'n' : _ -> return ()
        'N' : _ -> return ()
        _       -> wonGame gen

cheatGame :: StdGen -> Game -> IO ()
cheatGame gen g = drawBrd (board g <> (mconcat . map shipToBrd . ships $ g)) >> playGame gen g

takeShot :: String -> StdGen -> Game -> IO ()
takeShot t gen g = let 
    r : c : _ = map (read) (words t)
    b = board g <> fillRectangle (lastChar 'X') 1 1 (coordI r c) <> (mconcat . map shipToBrd $ hit)
    hit = filter (isJust . getLast . (\q -> q (coordI r c)) . queryFill) (ships g)
    miss = filter (not . isJust . getLast . (\q -> q (coordI r c)) . queryFill) (ships g)
    in playGame gen (Game miss b)

playGame :: StdGen -> Game -> IO ()
playGame gen g = if win g then wonGame gen else do 
    drawBrd . board $ g    
    putStrLn "Guess r c / Cheat 'c' / New Game 'n' / Quit 'q'"
    t <- getLine
    case filter (not . isSpace) t of
        'c' : _ -> cheatGame gen g
        'C' : _ -> cheatGame gen g
        'n' : _ -> playNewGame gen
        'N' : _ -> playNewGame gen
        'Q' : _ -> return ()
        'q' : _ -> return ()
        _       -> takeShot t gen g
    where 
       win (Game [] _) = True
       win _ = False
    

main :: IO ()
main = do
    -- putStrLn "myPicture"
    -- myPicture
    -- putStrLn "myDone"
    -- myGameBoard
    -- myGameBoard2
    gen0 <- getStdGen
    -- let (gen1, b1) = randomBoard gen0
    -- drawFillMatrix 40 40 . mconcat $ b1
    -- let (gen2, b2) = randomBoard gen1
    -- drawFillMatrix 40 40 . mconcat $ b2
    playNewGame gen0
    return ()
\end{code}
