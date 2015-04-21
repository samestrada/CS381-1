module HW2 where

import Prelude hiding (Enum(..), sum)

type Numb = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Up | Down

data Expr = InpV Var
          | Numb
          | Add Expr Expr
    deriving(Show)

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
--  deriving (Show)

-- This macro will draw a line in Mini Logo syntax
line :: Cmd
line = Define "line" ["x1", "x2", "y1", "y2"] 
     [Pen Down, Move (InpV "x1", InpV "y1"),
     Move (InpV "x2", InpV "x2"), Pen Down]
