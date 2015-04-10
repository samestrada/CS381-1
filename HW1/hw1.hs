module HW1 where

import Prelude hiding (Enum(..), sum)

--
-- * Part 1: Natural numbers
--

-- | The natural numbers.
data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)

-- | The number 0.
zero :: Nat
zero = Zero

-- | The number 1.
one :: Nat
one = Succ zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--
--   >>> pred zero
--   Zero
--
--   >>> pred three
--   Succ (Succ Zero)
--
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ x) = x

-- | True if the given value is zero.
--
--   >>> isZero zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

-- | Convert a natural number to an integer.
--
--   >>> toInt zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
--toInt x xs = x 
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub Zero _ = Zero
sub n Zero = n
sub n m = sub (pred n) (pred m)


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt Zero _ = False
gt _ Zero = True
gt n m = gt (pred n) (pred m)


-- | Multiply two natural numbers.
--
--   >>> mult two zero
--   Zero
--
--   >>> mult zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult = undefined


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--
--   >>> sum [one,zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum = undefined


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat] -> [Nat]
odds = undefined



--
-- * Part 2: Run-length lists
--


-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
--
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
compress :: Eq a => [a] -> [(Int,a)]
compress = undefined


-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--
decompress :: [(Int,a)] -> [a]
decompress = undefined
