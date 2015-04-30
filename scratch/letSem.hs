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

-- let x = 3 in (x + x)
x1 :: Exp
x1 = Let "x" (Lit 3) (Add (Ref "x") (Ref "x"))

-- 4 + (Let y = 5 in (let z = 6 in (y + z)))
x2 :: Exp
x2 = Add (Lit 4) (Let "y" (Lit 5) (Let "z" (Lit 6) (Add (Ref "y") (Ref "z"))))

-- let x = 3 in (let x = 4 in x) + x
x3 :: Exp
x3 = Let "x" (Lit 3) (Add (Let "x" (Lit 4) (Ref "x")) (Ref "x"))

--
-- 2. Identify Semantic Domain
--

type Env = [(Var,Int)]
type Domain = Env -> Maybe Int 

--
-- 3. Define Semantic function
--

sem :: Exp -> Env -> Maybe Int
sem (Lit i)     _ = Just i
sem (Add l r)   m = case (sem l m, sem r m) of
                         (Just i, Just j) -> Just (i + j)
                         _                -> Nothing
sem (Let v l r) m = case sem l m of 
                    Just i -> sem r ((v,i):m)
                    _      -> Nothing
sem (Ref v)     m = lookup v m

--
-- >>> sem x1
-- Just 6
--
-- >>> sem x2
-- Just 15
--
-- sem x3
-- Just 
--
