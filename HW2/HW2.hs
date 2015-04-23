-- Name: Brandon Burgess
-- ID: *** *** ***

module HW2 where

import Prelude

--Part 1
type Numb = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up | Down
  deriving (Eq, Show)

--InpV for Input variable and InpN for Input Number
data Expr = InpV Var
          | InpN Numb
          | Add Expr Expr
    deriving(Eq, Show)

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq, Show)

--Part 2
-- This macro will draw a line in Mini Logo syntax
-- This will conform to the following concrete syntax
-- Pen Up
-- Move (x1, y1)
-- Pen Down
-- Move (x2, y2)
-- Pen Up
line :: Cmd
line = Define "line" ["x1", "x2", "y1", "y2"]
     [Pen Up, Move (InpV "x1", InpV "y1"), Pen Down,
     Move (InpV "x2", InpV "x2"), Pen Up]

--Part 3
-- This Macro will draw an X beginning at x,y with height h and width w
-- This will conform to the following concrete syntax
-- Define nix [x,y,w,h]
-- Call line (x, y, x+w, h+y );
-- Call line (x+w, y, w, h+y );
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
     [Call "line" [InpV "x", InpV "y",
     Add (InpV "x") (InpV "w"),
     Add (InpV "h") (InpV "y")],
     Call "line" [Add (InpV "x") (InpV "w"),
     InpV "y", InpV "w",
     Add (InpV "h") (InpV "y")]]

--Part 4
step :: Int -> Prog
step x = [Pen Up, Move (InpN x, InpN x), Pen Down, Move (InpN x, InpN (x+1)),
          Move (InpN (x+1), InpN (x+1)), Pen Up]

steps :: Int -> Prog
steps 0 = []
steps x = step 1 ++ steps (x-1)

-- Using this to test Things. Grader should disregard
step_test :: Int -> Prog
step_test x = [Pen Up, Move (InpN x, InpN x), Pen Down, Move (InpN x, InpN (x+1)),
              Move (InpN (x+1), InpN (x+1)), Pen Up,
              (Define "line" ["x1", "x2", "y1", "y2"]
              [Pen Up, Move (InpV "x1", InpV "y1"), Pen Down,
              Move (InpV "x2", InpV "x2"), Pen Up]),
              Call "line" [InpV "x", InpV "w", InpV "y", InpV "h"]]
-- Grader should resume regarding :P

--Part 5
macros :: Prog -> [Macro]
macros ((Define m _ _):xs) = m : macros xs
macros ((Pen _):xs) = macros xs
macros ((Move _):xs) =  macros xs
macros ((Call _ _):xs) = macros xs
macros _ = []

--Part 6
-- This is to take the list of Expressions in call and pretty print them.
-- Was unsure if we needed to remove quotes and if so how we were to do so.
toStrFroExpr :: [Expr] -> String
toStrFroExpr ((InpV x):xs) = 
            if (length xs) == 0
                then x
            else x ++ ", " ++ toStrFroExpr xs
toStrFroExpr ((InpN x):xs) = 
            if (length xs) == 0
                then show x
            else show x ++ ", " ++ toStrFroExpr xs
toStrFroExpr ((Add (InpV x) (InpV y)):xs) =
            if (length xs) == 0
                then x ++ "+" ++ y
            else x ++ "+" ++ y ++ ", " ++ toStrFroExpr xs
toStrFroExpr _ = ""

-- The "real" part of part 6
pretty :: Prog -> String

pretty ((Define m v p):xs) =
            if (length xs) == 0
                then pretty p ++ "; "
            else "Define " ++ m ++ " " ++ (show v)
                ++ "{\n" ++ pretty p ++ "\n}"
                ++ "; \n" ++ pretty xs

pretty ((Pen m):xs) =
            if (length xs) == 0
                then "Pen " ++ show m ++ "; "
            else ("Pen " ++ show m ++ "; \n" ++ pretty xs)

pretty ((Move ((InpN x),(InpN y))):xs) =
            if (length xs) == 0
                then "Move (" ++ show x ++ ", " ++ show y ++ "); "
            else ("Move (" ++ show x ++ ", " ++ show y ++ "); \n" ++ pretty xs)

pretty ((Move ((InpV x),(InpV y))):xs) =
            if (length xs) == 0
                then "Move (" ++ x ++ ", " ++ y ++ "); "
            else ("Move (" ++ x ++ ", " ++ y ++ "); \n" ++ pretty xs)

pretty ((Call x y):xs) =
            if (length xs) == 0
                then "Call " ++ x ++ " (" ++ toStrFroExpr y ++ "); "
            else ("Call " ++ x ++ " (" ++ toStrFroExpr y ++ "); \n"
                ++ pretty xs)

pretty _ = ""
