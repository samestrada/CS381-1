module Let where

--Grammar for Let (concrete)
--  int ::= Integer
--  var ::= (Any variable name)
--  exp ::= Int                             Integer Literal
--      |   exp `+` exp                     Addition
--      |   `let` var `=` exp `in` exp      Variable declaration
--      |   var                             Variable Reference

-- 1. Encode the above grammar as a set of Haskell Data Types
type Var = String

-- data NameOfType = Constructor TypeArgs
--                 | Constructor TypeArgs
--                 | Constructor TypeArgs
--                 | etc...

data Exp = Lit Int
         | Add Exp Exp
         | Let Var Exp Exp      --Let a var 
         | Ref Var              --reference to variable
    deriving(Eq, Show)

-- 2. Write haskell values that represent some programs
-- let x = 3 in (x + x)
x1 :: Exp
x1 = Let "x" (Lit 3) (Add (Ref "x") (Ref "x"))

-- 4 + (Let y = 5 in (let z = 6 in (y + z)))
x2 :: Exp
x2 = Add (Lit 4) (Let "y" (Lit 5) (Let "z" (Lit 6) (Add (Ref "y") (Ref "z"))))


-- 3. Write a program that returns all the variables declared in a
-- Let program
-- TODO: Apply this to Macro in the homework
vars :: Exp -> [Var]
vars (Let v e1 e2) = v : (vars e1 ++ vars e2)
vars (Add e1 e2) = vars e1 ++ vars e2
vars (Lit _) = []
vars (Ref _) = []
