% Haskell Basics and Typeclassopedia
% Handré Stolp
% January 13, 2014

Introduction
==========
* First few type classes from the Typeclassopedia (Functor, Applicative and Monoid)
    * Based on ideas from mathematics
    * But don't be scared you don't need to be a mathematician
    * It just means its well founded and you can build intuitively on it
* Brief look at theory
* Hands on example where the theory is put to use
* Sample application making use of Functor, Applicative and Monoid
    * A basic ASCII art renderer
    * A basic battleship / mine sweeper game 
* This is a literate Haskell file
    * Can press `A` and copy all the text to a `.lhs` file and run in GHCi
* Will try and cover a little bit of Haskell introduction / syntax


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

Typeclassopdedia Diagram
===========
* The following is verbatim from <http://www.haskell.org/haskellwiki/Typeclassopedia>
![Diagram of Typeclassopedia](Typeclassopedia-diagram.png)
* Solid arrows point from the general to the specific; that is, if there is an arrow from Foo to Bar it means that every Bar is (or should be, or can be made into) a Foo.
* Dotted arrows indicate some other sort of relationship.
* Monad and ArrowApply are equivalent.
* Semigroup, Apply and Comonad are greyed out since they are not actually (yet?) in the standard Haskell libraries 

Functor
===========
* Most ubiquitous of Haskell typeclasses
* Intuition 1 : 
    * `Functor` is a "container"
    * that can map a function over all elements
    * not changing the structure
* Intuition 2 : 
    * `Functor` represents some "computational context" 
    * with the ability to "lift" functions into the context.
* Its definition : 
````haskell
class Functor f where
  -- Take function (a -> b) and return function f a -> f b
  fmap :: (a -> b) -> f a -> f b
````
* infix operator `<$>` is a synonym for `fmap` ie 
```haskell
g <$> x == g `fmap` x == fmap g x
```

-------

* The laws:
    * mapping the identity function over every item in a container has no effect. 
```haskell
fmap id = id
```
    *  mapping a composition of two functions over every item in a container is the same as first mapping one function, and then mapping the other.
```haskell
fmap (g . h) = (fmap g) . (fmap h)
```
* Notable instances
    * `(->) e` or functions `(e -> a)` are functors with element/contextual values of type `a`
    * `IO` so you can modify the results of monadic actions using `fmap`

Applicative
=============
* Lies between `Functor` and `Monad`
* `Functor` lifts a "normal" function to some context but does not allow applying a function in a context to a value in a context
* `Applicative` provides this by the lifted function application operator `<*>` 
* Additionally provides `pure` to embed a value in an "effect free" context.
```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
* `<*>` takes a function in context `f` and applies it to a value in context `f` returning a value in context `f`
* `<*>` is similar to a lifted `$`

----

* The laws:
    * The identity law:
```haskell
pure id <*> v = v
````
    * Homomorphism: Applying a non-effectful function to a non-effectful argument is the same as applying the function to the argument and then injecting into the context.
```haskell
pure f <*> pure x = pure (f x)
```
    * Interchange: When evaluating the application of an effectful function to a pure argument, the order does not matter
```haskell
u <*> pure y = pure ($ y) <*> u
```
    * Composition: The trickiest law to gain intuition for. Expressing a sort of associativity `<*>`
```haskell
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
```
    * relation to `fmap`: `fmap g x` is the same as lifting `g` using `pure` and applying to `x`
```haskell
fmap g x = pure g <*> x
```

Monoid
==============
* Extension of a semigroup (not covered here)
* A monoid has 
    * some associative binary operator i.e. `(a (+) b) (+) c == a (+) (b (+) c)`
    * which does not have to be commutative i.e. `a (+) b == b (+) a` NOT REQUIRED.
    * and which has some zero element related to the binary operator i.e. `a + 0 == a == 0 + a`
* Think in terms of list concatenation or and accumulator  

````haskell
class Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
 
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
````

* `mempty` is the empty element
* `mappend` is the binary associative operation between two elements
* `mconcat` is a convenience function which may be specialized and used to collapse a list of values using `mempty` and `mappend`
* `<>` infix operator is a synonym for `mappend` ie `a <> b == mappend a b`

----

* The laws

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

* Notable instances:
    * `Monoid b => Monoid (a -> b)` or any function from `a` to `b` where `b` is a `Monoid` is also a `Monoid`
        * The concatenation of a bunch of these functions essentially passes the same value to them all and combines 
          result using `Monoid` instance of `b`


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
* Two primitives, a circle and a rectangle
* Both primitives require `(Real c, Divisor c, Monoid a)`
    * that the coordinate element type be real and divisable (for `Monoid` instance of bounds)
    * and that `Monoid` instance exists for result value of the fill
* Circle takes some value a radius and a position and produces a value when the coordinate is within the radius
* Rectangle takes some value a width, height and a position and produces a value when the coordinate is within the bounds

\begin{code}
fillCircle :: (Real c, Divisor c, Monoid a) => a -> c -> Coord c -> Fill c a
fillCircle val radius pos = Fill qry bnds mv
    where
    -- When the coordinate is within the radius distance from the centre produce
    qry crd | coordLength (crd |-| pos) <= realToFrac radius  = val
            | otherwise                                       = mempty
    -- The bounds is a square centred around the position
    bnds = Bounds pos (Extents . Coord $ (radius, radius))
    -- Moving it construct circle with new centre
    mv pos' = fillCircle val radius (pos |+| pos') 

fillRectangle :: (Real c, Divisor c, Monoid a) => a -> c -> c -> Coord c -> Fill c a
fillRectangle val w h pos = Fill qry bnds mv
    where
    -- When the coordinate is within bounds of the rectangle produce
    qry crd | let (x, y) = unCoord $ abs <$> (crd |-| pos) 
                  in x <= halfW && y <= halfH       = val
            | otherwise                             = mempty
    halfW = w `divideBy'` 2
    halfH = h `divideBy'` 2
    -- the rectangle is its bounds
    bnds = Bounds pos (Extents . Coord $ (halfW, halfH))
    -- Moving it constructs a new rectangle centred on the new position
    mv pos' = fillRectangle val w h (pos |+| pos') 

\end{code}

Drawing ASCII
===================
* We can draw to the text buffer any `Fill` for which the produced value can map to a character
* We embody it through the `ProduceChar` type class
* We add convenience instances for 
    * `Char` 
    * Any `Maybe` type for which `a` embodies `ProduceChar`
    * Any `Last` type for which `a` embodies `ProduceChar`
* `Last` is a `newtype` wrapper around `Maybe` giving a `Monoid` instance taking the last produced value if any.
* Since we are going to use `Last Char` a lot we add a helper for it

\begin{code}
class ProduceChar a where produceChar :: a -> Char -- map some value to a Char

instance ProduceChar Char where 
    produceChar = id    -- always produces itself (hence id)

instance ProduceChar a => ProduceChar (Maybe a) where
    produceChar Nothing   = ' '             -- when nothing produce space
    produceChar (Just a) = produceChar a    -- when something produce the related Char

instance ProduceChar a => ProduceChar (Last a) where 
    produceChar = produceChar . getLast

lastChar :: Char -> Last Char
lastChar = Last . Just
\end{code}

-----

* We then draw a matrix of character to standard out by querying each character position for the produced char
* It takes a width, a height and some `Fill` to say how the matrix should be filled
* It draws 2 characters per column to get a more square looking picture
* Notice that to turn a fill into something that produces characters we just `fmap` the function `produceChar` over the `Fill`

\begin{code}
drawFillMatrix :: ProduceChar a => Int -> Int -> Fill Int a -> IO ()
drawFillMatrix cs ls fl = putStrLn $ ios cs2 ls
    where
        cs2      = cs * 2
        -- we map produceChar over the result of the query
        flToChar = queryFill . fmap produceChar $ fl    
        ios 0 0  = []
        -- end of each line we add a new line
        ios 0 l  = '\n' : ios cs2 (l-1)
        -- we iterate over the coordinates in our matrix
        ios c l  = (flToChar $ Coord (cs - c `div` 2, ls - l)) : ios (c-1) l

\end{code}


Example Picture
===================
* Here is an example picture being drawn
* It draws `'+'` in the corners of the character matrix
* Then draws the layering of a circle of `'X'` then `'#'`, then rectangle of `'$'` and finally a circle of `' '`
* The whole circle is also move 5 spaces left and down
* Notice that we layer the `Fill` values using the `Monoid` append operator `<>` 
    * The result is one `Fill` that maps different coordinates to different characters

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

-----

![Example ASCII image](ExampleImage.png)


Battleship
===================
* For our battle ship / mine sweeper game 
* We are going to define areas with ships in them
* The user must fire shots at specific coordinates
* When he hits a ship it gets revealed
* When he gets them all he wins
* Two ship types destroyers and cruisers
* Destroyers drawn as 'D'
* Cruiser drawn as 'C'
* Guesses drawn as 'X'
* '+' Drawn in the corners of the map

----


![Example battleship board](ExampleBattleShipBoard.PNG)

Defining ships
===================
* A ship can be either a `Cruiser` or a `Destroyer`
* `Cruiser` is drawn as the character 'C'
* `Destroyer` is drawn as the character 'D'
* Ships can be oriented `ShipVertical` or `ShipHorizontal`

\begin{code}
-- Type of ship
data ShipType = Cruiser | Destroyer

-- what character is produced by the ship type
instance ProduceChar ShipType where
    produceChar Cruiser = 'C'
    produceChar Destroyer = 'D'

-- How is the ship oriented on the board
data ShipOrientation = ShipVertical | ShipHorizontal deriving (Show, Eq, Ord, Bounded)
\end{code}

-------

* A `cruiser` is a square with a 'circle' at the one end (looks like an arrow)
* A `destroyer` is a thinner square with 'circle' at either ends
* Notice that we define the areas covered by ships using the `Monoid` append operator `<>` over `Fill` areas
* The resultant `Fill` types produce respective `ShipType` values for the areas where they are defined

\begin{code}
-- Make a cruiser ship given an orientation and a centre
cruiser :: ShipOrientation -> CoordI -> Fill Int (Last ShipType)
cruiser o pos = case o of
    ShipVertical    -> fillRectangle t 2 3 pos <> fillCircle t 2 (pos |-| coordI 0 3)
    ShipHorizontal  -> fillRectangle t 3 2 pos <> fillCircle t 2 (pos |-| coordI 3 0)
    where
        t = Last . Just $ Cruiser

-- Make a destroyer ship given an orientation and a centre
destroyer :: ShipOrientation -> CoordI -> Fill Int (Last ShipType)
destroyer o pos = case o of
    ShipVertical    -> fillRectangle t 1 2 pos 
                    <> fillCircle t 2 (pos |-| coordI 0 3) 
                    <> fillCircle t 2 (pos |+| coordI 0 3)

    ShipHorizontal  -> fillRectangle t 2 1 pos 
                    <> fillCircle t 2 (pos |-| coordI 3 0) 
                    <> fillCircle t 2 (pos |+| coordI 3 0)
    where
        t = Last . Just $ Destroyer
\end{code}

Laying out the board
===================
* Given a board size and a list of ships
    * we want to arrange the ships so that they are inside the boards bounds
    * and so that no two ships overlap
* It uses the bounds of different ships as well as query at which coordinates they are producing values
* Big ugly function with some bugs (enough said)

-----

\begin{code}
layoutBoard :: Int -> Int -> [Fill Int (Last ShipType)] -> [Fill Int (Last ShipType)]
layoutBoard _ _ [] = []
layoutBoard w h ships = ships'
    where
        ships' = foldl findPlace [] shipsInBnds
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
\end{code}


Making a random board
===================
* We want to create a random layout of 5 ships
* We take a random number generator and return a modified generator along with the list of ships
    * Returning the modified generator allows repeated calls to generate different lists
* We use the `newtype` wrapper over lists `ZipList` which gives an alternate `Applicative` implementation for lists.
    * Combines list by zipping them together and not by combining all possible combinations of values
* Take note of `([destroyer, cruiser] !!)` where the list index operator is partially applied to a list of functions.
    * It is then mapped to a infinite random list of [0,1] values.
* Same trick used to generate infinite list of orientations
* We also have infinite lists of 'x' and 'y' coordinates
* We use `Applicative` to combine infinite lists of functions over infinite lists of values to give infinite list of resultant ships.
* But we only take 5 of the resulting values and return them.
* In the end we pass the list through `layoutBoard` to make sure its is a valid configuration.

\begin{code}
randomBoard :: StdGen -> (StdGen, [Fill Int (Last ShipType)])
randomBoard gen = 
    let ship = ZipList . map ([destroyer, cruiser] !!) . randomRs (0, 1) $ gen
        orient = ZipList . map ([ShipVertical,ShipHorizontal] !!) . randomRs (0, 1) $ gen
        cxs = ZipList . randomRs (0, 40) $ gen
        cys = ZipList . randomRs (0, 40) $ gen
    in ( mkStdGen . fst . random $ gen
       , layoutBoard 40 40 . take 5 . getZipList $ ship <*> orient <*> (coordI <$> cxs <*> cys)
       )

\end{code}
       
Managing the game
===================
* Now that we can define ships and draw them we need to manage the flow of our game
* We will do that by passing around `Game` state value between different IO actions
* We keep track of alive ships and have a board on which we draw the guesses as well as dead ships

\begin{code}
data Game = Game { ships     :: [Fill Int (Last ShipType)] -- the alive ships
                 , board     :: Fill Int (Last Char)       -- the board showing choices and dead ships
                 }

-- Helper that maps a ship to characters
shipToBrd :: Fill Int (Last ShipType) -> Fill Int (Last Char)
shipToBrd s = Last . (fmap produceChar) . getLast <$> s

-- Helper action that draws the game board for us
drawBrd :: Fill Int (Last Char) -> IO ()
drawBrd = drawFillMatrix 40 40  

\end{code}

----

* We creating a new game we use `Applicitive` again
* `(coordI <$> [0,40] <*> [0,40])` applies `coordI` to all the possible combinations of corner coordinates

\begin{code}

-- Action starting a new game
playNewGame :: StdGen -> IO ()
playNewGame gen = let 
                  -- we generate a random board
                  (gen', ships) = randomBoard gen
                  -- draw the border '+' characters using the normal Applicative instance for list
                  -- to get all the corner combinations
                  border = mconcat $ map (fillRectangle (lastChar '+') 1 1) (coordI <$> [0,40] <*> [0,40]) 
                  -- and then we start the game
                  in playGame gen' (Game ships border)
\end{code}

-----
* When we cheat in a game we momentarily show everything. 
    * We use the `Monoid` append operator `<>` to combined the current `board`
    * with the list of `ships` flattened to a displayable `Fill` using
    * `mconcat` which flattens a list using `Monoid`

\begin{code}
-- when we chose to cheat we show the board with all the ships on it and then continue playing
cheatGame :: StdGen -> Game -> IO ()
cheatGame gen g = drawBrd (board g <> (mconcat . map shipToBrd . ships $ g)) >> playGame gen g

-- when we win the game we can choice to play a new one
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
\end{code}

----

* When we take a shot we again use `Monoid` append operator `<>` to update the board
    * We include the location of our guess using 'X'
    * and we also include the locations of all the hit ships.
    * The list of hit ships is collapsed into a single `Fill` using `Monoid`

\begin{code}
takeShot :: String -> StdGen -> Game -> IO ()
takeShot t gen g = let 
    r : c : _ = map (read) (words t)
    -- The board is updated
    b = board g 
        -- With an 'X' showing where we guessed
        <> fillRectangle (lastChar 'X') 1 1 (coordI r c) 
        -- And the display of all the ships which were hit
        <> (mconcat . map shipToBrd $ hit)
    -- Hit ships are those which produce at the coordinate
    produces = isJust . getLast . (\q -> q (coordI r c)) . queryFill
    hit = filter produces (ships g)
    -- Missed ships are those which do not produce at the coordinate
    miss = filter (not . produces) (ships g)
    -- The new game state is all the missed ships and the updated board
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
\end{code}

Main function entry point
=========================

\begin{code}
main :: IO ()
main = do
    putStrLn "An exmaple picture"
    myPicture
    _ <- putStrLn "enter any text to continue " >> getLine
    gen0 <- getStdGen
    playNewGame gen0
    return ()
\end{code}
