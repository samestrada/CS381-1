module IntBool where

-- 1. Define Abstract syntax
data Exp = Lit Int
         | Add Exp Exp
         | Equ Exp Exp
         | Not Exp
    deriving (Eq, Show)

--Example expressions
-- * Draw AST
-- * What is the result?
--x = Add (Lit 2) (Add (Lit 3) (Lit 4))
--y = Not (Equ x (Lit 10))
--z = Not x

--
-- 2. Define the syntax of types
--
--data Val = I Int | B Bool | TypeError

data Type = TInt | TBool | TypeError
    deriving(Eq, Show)

--
-- 3. Define typing relation
--

typeOf :: Exp -> Type
typeOf (Lit _)   = TInt
typeOf (Not e)   = case typeOf e of 
                    TBool -> TBool
                    _     -> TypeError
typeOf (Add l r) = case (typeOf l, typeOf r) of
                    (TInt, TInt) -> TInt
                    _            -> TypeError
typeOf (Equ l r) = case (typeOf l, typeOf r) of
                    (TInt, TInt)   -> TInt
                    (TBool, TBool) -> TBool
                    _              -> TypeError

--
-- 4. Define the semantics of type-correct program
--

sem :: Exp -> Either Int Bool
sem (Lit i) = Left i
sem (Not e) = let (Right b) = sem e in Right (not b)
sem (Add l r) = let Left i = sem l
                    Left j = sem r
                in Left (i+j)
sem (Equ l r) = Right (sem l == sem r)
