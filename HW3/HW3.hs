module HW3 where


--
-- * Part 1: Simple Stack Language
--

--
-- ** Syntax
--

-- | A program is a list of commands.
type Prog = [Cmd]

-- | A command in the language manipulates the stack.
data Cmd = Push Int      -- push an int onto the stack
         | Pop           -- pop the top int off the stack
         | Dup           -- duplicate the int on top of the stack
         | Swap          -- swap the top two ints on the stack
         | Add           -- add the top two ints on the stack
  deriving (Eq,Show)


--
-- ** Semantics
--

-- | A stack of integers.
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
--
cmd = undefined


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
--
prog = undefined


-- | Evaluate a program starting with an empty stack.
run = undefined


--
-- * Part 2: Adding Macros
--

--
-- ** Syntax
--

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
push n = Basic (Push n)
pop    = Basic Pop
dup    = Basic Dup
swap   = Basic Swap
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


--
-- ** Semantics
--

-- | Associates macro names with their definitions.
type Macros = [(Name,XProg)]

-- | The runtime state of our extended stack language.
type State = (Macros,Stack)


-- | Semantics of an extended command.
xcmd :: XCmd -> State -> Maybe State
xcmd = undefined


-- | Semantics of an extended program.
xprog :: XProg -> State -> Maybe State
xprog = undefined


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
xrun = undefined
