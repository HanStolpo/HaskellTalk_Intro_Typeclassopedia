% Haskell Basics and Typeclassopedia
% Handré Stolp
% January 13, 2014

Introduction
==========

What will be covered
-----------
* Basic syntax of Haskell
* First few type classes from the Typeclassopedia (Functor, Applicative and Monoid)

How is it presented
-----------------
* As literate Haskell
* Can press `A` and copy all the text to a `.lhs` file and run in GHCi
* Sample application making use of Functor, Applicative and Monoid
    * A basic ASCII art renderer
    * A basic battleship / mine sweeper game 


Begin file
==========
* File starts with a module declaration `module X where`
* Name of module must be same as file name (except if module contains `main` then may differ)
* Module definition optionally preceded by language extension pragma `{-# LANGAGE x #-}`
* Then import declarations `import x`

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
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

Coord Type
==========
* We want a type to hold screen (x,y) co-ordinates
* Instead of using the built-in tuple type we wrap it in a `newtype`
    * More control
    * Makes a new type that shares the wrapped type's runtime infrastructure
    * No runtime cost
    * May only have single value and single constructor
    * Type constructor `Coord` maps from `(a,a)` to `Coord`
    * record accessor `unCoord` maps from `(a,a)` to `Coord`
* We fix the tuple element types so that they are the same
* We define `show` for Coord to be the same `show` for tuple

\begin{code}
newtype Coord a = Coord {unCoord :: (a,a)} deriving (Eq, Ord)

instance Show a => Show (Coord a) where show = show . unCoord
\end{code}

Coord Functor
=============
* We give `Coord` a `Functor` instance
* It applies the function to each element
* Will allow us to lift functions to work on `Coord`
* Will come in useful later

\begin{code}
instance Functor Coord  where
    fmap f (Coord (x,y)) = Coord (f x, f y)
\end{code}

Coord Applicative
================
* `Applicative` instance allows us to apply lifted function to values in `Coord` context
* `pure` fills both elements with the same value
* `<*>` applies the functions in each element of LHS `Coord` to values in RHS `Coord` respectively

\begin{code}
instance Applicative Coord where
    pure a = Coord (a,a)
    Coord (g, h) <*> Coord (x, y) = Coord (g x, h y)
\end{code}

Coord Monoid
=============
* We only have a `Monoid` instance if the element type has one
* `mempty` is just `mempty` for the element type
* `mappend` is just `mappend` for the element type applied per element respectively

\begin{code}
instance Monoid a => Monoid (Coord a) where
   mempty = Coord (mempty, mempty)
   Coord (lx, ly) `mappend` Coord (rx, ry) = Coord (lx <> rx, ly <> ry)
\end{code}

Coord Operators
==========
* We add our own operators for adding and subtracting `Coord` values
* We restrict the operators to only be available if the element type is of class `Num`
    * `Num` gives access to `+` and `-`
* We give them the same operator precedence as `+` and `-`
* Because `Coord` is of class `Applicative` we can define the operations by lifting `+` and `-`
    * Lift `+` and apply to `a` (`(+) <$> a`)
    * Apply the partially applied function in `Coord` to `b` (`<*> b`)
* Clean and clear

\begin{code}
(|+|) :: Num a => Coord a -> Coord a -> Coord a
infixl 6 |+|
a |+| b = (+) <$> a <*> b

(|-|) :: Num a => Coord a -> Coord a -> Coord a
infixl 6 |-|
a |-| b = (-) <$> a <*> b
\end{code}

* Helper function to give the length of the co-ordinate
    * Notice `realToFrac`, its used to coerce any real number to a fractional (for `sqrt`)
\begin{code}
coordLength :: (Real a, Floating b) => Coord a -> b
coordLength (Coord (x, y)) = sqrt . realToFrac $ x * x + y * y
\end{code}

Extents Type
===========
* When working with rectangular bounds we want a centre and an extent
* An extent must always be positive and is half the width and height of the bounding rectangle
* But extents are usually going to be applied to `Coord` values
* We `newtype` wrap `Coord` to create `Extent`
    * This limits operations on `Extents` 
    * We ignore `Extents` data constrcutor and use `extentsFromCoord` which forces absolute values
    * Ideally you would not export the `Extents` constructor from the module
* `extentsFromCoord` maps `Coord` to `Extents`
* record member accessor `coordFromExtents` maps `Extents` to `Coord`
* Notice that we chain the `Extents` constructor with the lifted `abs` function over `Coord`

\begin{code}
newtype Extents a = Extents {coordFromExtents :: Coord a} deriving (Eq, Ord)

extentsFromCoord :: Num a => Coord a -> Extents a
extentsFromCoord c = Extents . fmap abs $ c

instance Show a => Show (Extents a) where show = show . coordFromExtents
\end{code}

Bounds Type
===========
* We represent a `Bounds` as a centre with an extents
* It is parametric in the element type of `Coord` and `Extents`
* We add a `Monoid` instance so that `Bounds` may accumulate into larger `Bounds`

\begin{code}
data Bounds a = Bounds { boundsCentre :: Coord a
                       , boundsExtent :: Extents a
                       }  deriving (Show, Eq, Ord)
\end{code}
* We need to specify how element types can be divided
* for this we add the `Divisor` class and specialize it for the numeric types

\begin{code}

class Divisor a where divideBy' :: a -> a -> a
instance Divisor Double where divideBy' = (/)
instance Divisor Float where divideBy' = (/)
instance Divisor Int where divideBy' = div
instance Divisor Integer where divideBy' = div
\end{code}

-----------------

* Our `Monoid` instance requires that the element type be in `Divisor`, `Num` and `Eq` so that all operations can be performed.
* Empty bounds has zero extents
* The 'sum' of 2 bounds is the average of their centres and the sum of their extents
* Since `Coord` is `Applicative` notice how we can lift `divideBy` to work on the result of `|+|`

\begin{code}
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
\end{code}

Convenience Integer Typedefs
==============================
* `coordI` constructor for `Int` based `Coord` values.

\begin{code}
type CoordI = Coord Int
type ExtentsI = Extents Int
type BoundsI = Bounds Int

coordI :: Int -> Int -> Coord Int
coordI x y = Coord (x,y)
\end{code}

Fill Type
==============================
* We want to define how to draw to 2D area
* We also want to associate arbitrary data with 2D area
* We define the data type `Fill` based on some `Coord` element type `c` and some value `a`
    * It fills a 2D area with values : `queryFill` maps `Coord` inputs to some value `a`
    * It has an associated bounds : `fillBounds`
    * It is possible to move a `Fill` around : `moveFill`

\begin{code}
data Fill c a = Fill  { queryFill  :: Coord c -> a
                      , fillBounds :: Bounds c
                      , moveFill   :: Coord c -> Fill c a
                      }
\end{code}

Fill Functor and Monoid
=========================
* We make `Fill c` a `Functor` so that the associated values may be modified.
* The functor instance retains the bounds (i.e. position does not change)
* Since `(->) a` is an instance of `Functor` we just `fmap` `g` over `q` to get the modified query.
    * Changes the output of the current query by passing it through the function being mapped.
* Similarly `moveFill` is a `Functor` but its result is also a `Fucntor` so we just lift the function twice to get the new move.

\begin{code}
instance Functor (Fill c) where
    fmap g Fill  { queryFill = q
                 , fillBounds = b
                 , moveFill = m
                 } = Fill (fmap g q)            -- map g over q to get new query
                          b 
                          ((fmap . fmap) g m)   -- lift g twice before applying to m to the new move function
\end{code}
* `Fill` has a `Monoid` instance given that 
    * `Bounds` has a `Monoid` instance for the co-ordinate type `c`
    * and the value type `a` has a `Monoid` instance
* Since `(->) a` has a `Monoid` instance just 'concat' the query functions and the move functions

\begin{code}
instance (Monoid a, Monoid (Bounds c)) => Monoid (Fill c a) where
    mempty = Fill (const mempty) mempty (const mempty)
    a `mappend` b = Fill (queryFill a <> queryFill b)       -- concat the result of the query
                         (fillBounds a <> fillBounds b)     -- sum the bounds
                         (moveFill a <> moveFill b)         -- concat the results of the move
\end{code}

Filling Primitives
===================

\begin{code}
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

\end{code}

Drawing ASCII
===================

\begin{code}
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
\end{code}


Example Picture
===================

\begin{code}
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
\end{code}

Battle ship
===================

\begin{code}
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
