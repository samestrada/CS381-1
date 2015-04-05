-- | Factorial for reallly reallllllly big numbers (i can do factorial 10000)
factorial :: Integer -> Integer
factorial n = product[1..n]

-- | Circumfrence of a circle with floats
circumfrence :: Float -> Float
circumfrence f = 2 * pi * f

-- | Circumfrence of a circle with doubles
circumfrence' :: Double -> Double
circumfrence' d = 2 * pi * d

-- | Is this integer even?
even :: Int -> Bool
even a = (mod a 2) == 0

-- | Is this integer odd?
odd :: Int -> Bool
odd a = (mod a 2) == 1
odd' a = not . Main.even

-- | Is this integer zero?
isZero :: Int -> Bool
isZero a = if a > 0 then False else if a < 0 then False else True
-- | Pattern matching
isZero' :: Int -> Bool
isZero' 0 = True
isZero' _ = False


