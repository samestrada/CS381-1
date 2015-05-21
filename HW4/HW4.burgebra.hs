module HW4 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs has the definitions of Mode, Cmd, and Prog!
--   * Render.hs has the definitions of Point and Line!

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Down) (_,(x,y)) = ((Down,(x,y)), Nothing)
cmd (Pen Up) (_,(x,y)) = ((Up,(x,y)), Nothing)
cmd (Move x y) (m,(a,b)) = case m of
                           Up -> ((Up,(x,y)), Nothing)
                           Down -> ((Down, (x,y)), (Just ((a,b),(x,y))))

getLines :: Prog -> State -> [Line]
getLines [] (_,_)  = []
getLines [x] (i,y) = case x of
                     Pen Down -> []
                     Pen Up -> []
                     Move u v -> case i of
                                 Up -> []
                                 Down -> [(y,(u,v))]
getLines (x:xs) (i,y) = case x of
                 Pen Down -> getLines xs (Down, y)
                 Pen Up -> getLines xs (Up, y)
                 Move u v -> case i of
                            Up -> getLines xs (Up, (u,v))
                            Down ->  (y,(u,v)):(getLines xs (Down, (u,v)))


getFinalState :: Prog -> State -> State
getFinalState [] (i,y)  = (i,y)
getFinalState [x] (i,y) = case x of
                     Pen Down -> getFinalState [] (Down,y)
                     Pen Up -> getFinalState [] (Up,y)
                     Move u v -> (i,(u,v))
getFinalState (x:xs) (i,y) = case x of
                 Pen Down -> getFinalState xs (Down, y)
                 Pen Up -> getFinalState xs (Up, y)
                 Move u v -> case i of
                            Up -> getFinalState xs (Up, (u,v))
                            Down ->  getFinalState xs (Down, (u,v))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog x y = ((getFinalState x y), (getLines x y))

--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = undefined


