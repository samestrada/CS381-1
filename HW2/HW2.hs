module HW2 where

import Prelude hiding (Enum(..), sum)

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
steps _ = undefined 
        {-I wanna do something like:
        step x steps (x-1)
        but for some reason GHC applies all x steps and (x-1) to step
        which doesn't make any sense to me-}

macros :: Prog -> [Macro]
macros = undefined

pretty :: Prog -> String
pretty = undefined
