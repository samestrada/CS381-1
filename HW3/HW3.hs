module HW3 where

-- * Part 1: Simple Stack Language

-- ** Syntax

-- | A program is a list of commands.
type Prog = [Cmd]

-- | A command in the language manipulates the stack.
data Cmd = Push Int      -- push an int onto the stack
         | Pop           -- pop the top int off the stack
         | Dup           -- duplicate the int on top of the stack
         | Swap          -- swap the top two ints on the stack
         | Add           -- add the top two ints on the stack
  deriving (Eq,Show)

-- ** Semantics

-- | A stack of integers.
-- | This is our "environment, if you will
type Stack = [Int]

-- | Denotational semantics of a command.
--
--   >>> cmd (Push 4) [1,2,3]
--   Just [4,1,2,3]
--
--   >>> cmd Pop [1,2,3]
--   Just [2,3]
--
--   >>> cmd Dup [1,2,3]
--   Just [1,1,2,3]
-- 
--   >>> cmd Swap [4,5,6]
--   Just [5,4,6]
--
--   >>> cmd Add [4,5,6]
--   Just [9,6]
--
--   >>> cmd Pop []
--   Nothing
--
--   >>> cmd Dup []
--   Nothing
--
--   >>> cmd Swap []
--   Nothing
--
--   >>> cmd Swap [1]
--   Nothing
--
--   >>> cmd Add []
--   Nothing
--
--   >>> cmd Add [1]
--   Nothing

cmd :: Cmd -> Stack -> Maybe Stack
cmd (Push i) xs     = Just (i : xs)
cmd (Pop) (_:xs)    = Just xs
cmd (Dup) (x:xs)    = Just ([x,x] ++ xs)
cmd (Swap) (x:y:xs) = Just (y:x:xs)
cmd (Add) (x:y:xs)  = Just ((x+y):xs)
cmd _ _             = Nothing

-- | Denotational semantics of a program.
--
--   >>> prog [Push 3,Add] [5]
--   Just [8]
--
--   >>> prog [Dup,Pop] [5]
--   Just [5]
--
--   >>> prog [Dup,Add] [5]
--   Just [10]
--
--   >>> prog [Swap,Pop,Dup,Add] [5,3]
--   Just [10]
--
--   >>> prog [Pop,Dup] [5]
--   Nothing

prog :: [Cmd] -> Stack -> Maybe Stack
prog ([x]) y  = cmd x y
prog (x:xs) ys = prog xs (unMaybe (cmd x ys))
prog [] _      = Nothing

-- | Evaluate a program starting with an empty stack.
run :: Prog -> Stack -> Maybe Stack
run x [] = prog x []
run _ _  = Nothing

-- * Part 2: Adding Macros

-- ** Syntax

-- | A macro name.
type Name = String

-- | Extended version of Prog that supports macros.
type XProg = [XCmd]

-- | Extended version of Cmd that supports macros.
data XCmd = Define Name XProg   -- define a macro
          | Call Name           -- call a macro
          | Basic Cmd           -- a basic command
  deriving (Eq,Show)

-- Some aliases for basic commands.
push   :: Int -> XCmd
push n = Basic (Push n)
pop    :: XCmd
pop    = Basic Pop
dup    :: XCmd
dup    = Basic Dup
swap   :: XCmd
swap   = Basic Swap
add    :: XCmd
add    = Basic Add

-- Some example macro definitions:

-- | A macro "double" that doubles the argument on top of the stack.
double :: XCmd
double = Define "double" [dup,add]

-- | A macro "triple" that triples the argument on top of the stack.
triple :: XCmd
triple = Define "triple" [dup,dup,add,add]

-- | A macro that uses "double" and "triple" (i.e. it assumes they
--   have already been defined).
trouble :: XCmd
trouble = Define "trouble" [dup, Call "triple", swap, Call "double"]

-- ** Semantics

-- | Associates macro names with their definitions.
type Macros = [(Name,XProg)]

-- | The runtime state of our extended stack language.
type State = (Macros,Stack)


-- | Semantics of an extended command.
xcmd :: XCmd -> State -> Maybe State
xcmd (Basic c)  (m, []) = case c of
                          Swap -> Nothing
                          Dup -> Nothing
                          Pop -> Nothing
                          Add -> Nothing
                          Push x -> Just (m, unMaybe (cmd (Push x) []))
xcmd (Basic c) (m, [n]) = case c of
                          Swap -> Nothing
                          Add -> Nothing
                          _ -> Just (m, unMaybe (cmd c [n]))
xcmd (Basic c) (m,s)    = Just (m, unMaybe (cmd c s))
xcmd (Define n p) (m,s) = Just (m ++ [(n,p)], s)
xcmd (Call n) (m,s)     = xprog (unMaybe (lookup n m)) (m,s)


xprog :: XProg -> State -> Maybe State
xprog ([x]) y  = xcmd x y
xprog (x:xs) ys = xprog xs (unMaybe (xcmd x ys))
xprog [] _      = Nothing

-- | Evaluate a program starting with an empty stack.
--   
--   >>> xrun [double,push 3,triple,Call "double"]
--   Just [6]
--
--   >>> xrun [double,push 3,triple,Call "triple"]
--   Just [9]
--
--   >>> xrun [double,push 3,triple,trouble,Call "trouble"]
--   Just [6,9]
--
--   >>> xrun [push 3,trouble,Call "trouble"]
--   Nothing
--

xrun :: XProg -> Maybe Stack
xrun x = Just (snd (unMaybe (xprog x ([],[]))))

-- This maybe a function, I don't know anymore.
unMaybe :: Maybe t -> t
unMaybe (Just x) = x
unMaybe (Nothing) = undefined
