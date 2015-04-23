module HW2 where

import Prelude

type Numb = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up | Down
  deriving (Show)

--InpV for Input variable and InpN for Input Number
data Expr = InpV Var
          | InpN Numb
          | Add Expr Expr
    deriving(Show)

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Show)

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


nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
     [Call "line" [InpV "x", InpV "w", InpV "y", InpV "h"],
     Call "line" [InpV "x", InpV "y", InpV "w", InpV "h"]]

step :: Int -> Prog
step x = [Pen Up, Move (InpN x, InpN x), Pen Down, Move (InpN x, InpN (x+1)),
          Move (InpN (x+1), InpN (x+1)), Pen Up]

steps :: Int -> Prog
steps 0 = []
steps x = step 1 ++ steps (x-1)

-- Using this to test macros
step_test :: Int -> Prog
step_test x = [Pen Up, Move (InpN x, InpN x), Pen Down, Move (InpN x, InpN (x+1)),
              Move (InpN (x+1), InpN (x+1)), Pen Up, 
              (Define "line" ["x1", "x2", "y1", "y2"]
              [Pen Up, Move (InpV "x1", InpV "y1"), Pen Down,
              Move (InpV "x2", InpV "x2"), Pen Up])]

macros :: Prog -> [Macro]
macros ((Define m _ _):xs) = m : macros xs
macros ((Pen _):xs) = macros xs
macros ((Move _):xs) =  macros xs
macros ((Call _ _):xs) = macros xs
macros _ = []

-- FIXME:
-- TODO: *Define needs to have InpV and quotes removed and be prettier
--       *Define also is missing a trailing newline
--       *Solve why there is a trailing semicolon
--       *Remove InpN from from move
pretty :: Prog -> String
pretty ((Define m v p):xs) = ("Define " ++ show m ++ show v
        ++ pretty p ++ "; \n"++ pretty xs)

pretty ((Pen m):xs) = ("Pen " ++ show m ++ "; \n" ++ pretty xs)

pretty ((Move (x, y)):xs) = ("Move (" ++ show x ++ ", " ++ show y 
        ++ "); \n" ++ pretty xs)

pretty ((Call x y):xs) = ("Call (" ++ show x ++ ", " ++ show y
        ++ "); \n" ++ pretty xs)
pretty _ = ""
