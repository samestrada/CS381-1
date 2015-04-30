
module StackLang where


import Prelude hiding (EQ,If)


-- Grammar for StackLang:
-- 
--    num ::= (any number)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= int               push a number on the stack
--         |  bool              push a boolean on the stack
--         |  `add`             add the top two numbers the stack
--         |  `eq`              check whether the top two elements are equal
--         |  `if` prog prog    if the value on the top 


-- 1. Encode the above grammar as a set of Haskell data types
type Num = Int
type Prog = [Cmd]

data Cmd = LitI Int
         | LitB Bool
         | Add
         | EQ
         | If Prog Prog
  deriving (Eq,Show)


-- 2. Write a Haskell value that represents a StackLang program that:
--     * checks whether 3 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
myProg :: Prog
myProg = [LitI 3, LitI 4, EQ, If [LitI 5, LitI 6, Add] [LitB False]]


-- 3. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
genAdd2 :: Int -> Int -> Prog
genAdd2 x y = [LitI x, LitI y, Add, Add]


-- 4. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
genSum :: [Int] -> Prog
genSum []     = [LitI 0]
genSum (x:xs) = genSum xs ++ [LitI x, Add]

