
-- ***** Hailey Palmiter     931-958-219 *****
-- ***** Layne Nolli         931-823-301 *****
-- ***** Kirk Stennett       931-775-870 *****

-- **************************************

-- num      ::=	(any natural number)
-- var      ::=	(any variable name)
-- macro	::=	(any macro name)
--
-- prog	::=	ε   |   cmd ; prog      sequence of commands
--
-- mode	::=	down   |   up           pen status
--
-- expr	::=	var                     variable reference
--       |	num                     literal number
--       |	expr + expr             addition expression
--
-- cmd	::=	pen mode                               change pen mode
--       |	move ( expr , expr )                   move pen to a new position
--       |	define macro ( var* ) { prog }         define a macro
--       |	call macro ( expr* )                   invoke a macro

-- ***************************************

module HW2 where

import Prelude hiding (Num)

--
-- ********** PART 1 **********
--

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down | Up
    deriving(Eq, Show)

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
    deriving(Eq, Show)

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
    deriving(Eq, Show)

--
-- ********** PART 2 **********
--

-- MiniLogo Concrete Syntax
-- Draw a line from (x1, y1) to (x2, y2)
-- Pen Up;
-- Move (x1, y1);
-- Pen Down;
-- Move (x2, y2);

line :: Var -> Var -> Var -> Var -> Cmd
line x1 y1 x2 y2 = Define "line" ["x1", "y1", "x2", "y2"]
     [Pen Up, Move(Ref x1, Ref y1), Pen Down, Move(Ref x2, Ref y2)]

--
-- ********** PART 3 **********
--

-- MiniLogo Concrete Syntax
-- Draw an X starting at (x,y)
-- with height h and width w
-- Line (x, y, x+w, y+h);
-- Line (x+w, y, x, y+h);

nix :: Var -> Var -> Var -> Var -> Cmd
nix x y w h = Define "nix" ["x", "y", "w", "h"]
    [Call "line" [Ref x, Ref y, Add(Ref x) (Ref w), Add(Ref y) (Ref h)],
    Call "line" [Add(Ref x) (Ref w), Ref y, Ref x, Add(Ref y) (Ref h)]]

--
-- ********** PART 4 **********
--

step :: Int -> Prog
step n = [Pen Up, Move(Lit n, Lit n), Pen Down, Move(Lit n, Lit (n-1)),
        Move(Lit(n-1), Lit(n-1)), Pen Up]

steps :: Int -> Prog
steps 0 = []
steps n = step n ++ steps (n-1)

--
-- ********** PART 5 **********
--

macros :: Prog -> [Macro]
macros ((Define m _ _):xs) = m : macros xs
macros ((Pen _):xs) = macros xs
macros ((Move _):xs) =  macros xs
macros ((Call _ _):xs) = macros xs
macros _ = []

--
-- ********** PART 6 **********
--

-- FIXME:
-- TODO: *Define needs to have InpV and quotes removed and be prettier
--       *Define also is missing a trailing newline
--       *Solve why there is a trailing semicolon

pretty :: Prog -> String
pretty ((Define m v p):xs) = if (length xs) == 0 then pretty p ++ "; "
            else ("Define " ++ show m ++ show v ++ pretty p ++ "; \n"++ pretty xs)

pretty ((Pen m):xs) = if (length xs) == 0 then "Pen " ++ show m ++ "; "
            else ("Pen " ++ show m ++ "; \n" ++ pretty xs)

pretty ((Move ((Lit x),(Lit y))):xs) = if (length xs) == 0 then "Move (" ++ show x ++ ", " ++ show y ++ "); "
            else ("Move (" ++ show x ++ ", " ++ show y ++ "); \n" ++ pretty xs)

pretty ((Call x y):xs) = if (length xs) == 0 then "Call (" ++ show x ++ ", " ++ show y ++ "); "
            else ("Call (" ++ show x ++ ", " ++ show y ++ "); \n" ++ pretty xs)
pretty _ = ""

