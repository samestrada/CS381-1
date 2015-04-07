module Basics where

import Prelude hiding (even,odd,sum,product,map,foldr)

---------------------
-- Introduce Tools --
---------------------

-- * GHCi
--   * :help, :load, :quit, :type
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- * basic data types (Bool, Int, Float)
--   * arithmetic and boolean expressions
--   * if-then-else
-- * applying functions
--   * with one argument
--   * with multiple arguments
--   * infix vs. prefix application: operators are just functions!
--     * (+) x y = x + y
--     * f x y = x `f` y
-- * defining basic functions
--   * pattern matching
-- * anonymous functions


-- | Is this integer even?
even :: Int -> Bool
even x = (x `mod` 2) == 0


-- | Is this integer odd?
odd :: Int -> Bool
odd = not . even


-- | Is this number zero?
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False



-------------------------
-- Lists and Recursion --
-------------------------

-- * lists and strings
--   * cons, nil, and syntactic sugar
--   * recursive functions
--   * higher-order functions

-- nil := [] empty list
-- (:) is an operator that takes a value and a list which then adds the
-- val to the beginning of the list (cons operator)
-- [1, 2, 3] is equivalant to 1:[2,3] or 1:2:3:[]
-- :r in GHCi reloads modules
-- head returns first element of a list
-- tail returns everything but the first element of list

-- | an equivalent but basic encoding of lists
data List a = Nil
        |Cons a (List a)
    deriving (Eq, Show)

-- | Double all of the elements in a list.
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (x:xs) = (2*x) : doubleAll xs


-- | Flip all of the boolean values in a list.
notAll :: [Bool] -> [Bool]
notAll [] = []
-- suggested? notAll bs = map not bs
notAll (b:bs) = not b : notAll bs


-- | Compute the sum of the elements in a list.
sum :: [Int] -> Int
sum [] = 0
sum (a:as) = a + sum as
-- the second value can be read as "the rest of the list" (Excess or xs)
-- first value being the head of the list

-- | Compute the product of the elements in a list.
product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs

-- | Combination of sum and product, somehow.
foo :: [Int] -> Int
foo [] = 1
foo (x:xs)  | odd (length xs) = x + foo xs
            | otherwise       = x * foo xs
-- | foo (x:xs) = if odd (length xs) then x + foo xs else x * foo xs
--  foo(1:(2:(3:[])))
--  1 * foo (2:(3:[]))
--  1 * (2 + foo (3:[]))
--  1 * (2 + (3 * foo []))
--  5
----------------------------
-- Higher-Order Functions --
----------------------------

-- map takes a list and returns a list with an operation performed on
-- each element of the list

-- * map and fold


-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

doubleAll' :: [Int] -> [Int]
doubleAll'  = map (2*) --eta reduction

notAll' :: [Bool] -> [Bool]
notAll'  = map not

-- | Fold a function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
-- f is accumulator function and y is base value
foldr _ y []     = y
foldr f y (x:xs) = f x (foldr f y xs)

sum' :: [Int] -> Int
sum' = foldr (+) 0

product' :: [Int] -> Int
product'   = foldr (*) 1


-- | other work
head :: [a] -> a
head [] = error "empty list!"
head (x:xs) = x

tail :: [a] -> a
tail [] = error "empty list!"
tail (x:xs) = xs

last :: [a] -> a
last = Basics.head . reverse

last' :: [a] -> a
last' [] = error "empty list!"
last' (x:[]) = x
last' (x:xs) = Basics.last xs
