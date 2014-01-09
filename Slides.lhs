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
module Slides where
\end{code}

Main
==========
Here is the main function
\begin{code}
main :: IO ()
main = putStrLn "Slides"
\end{code}
