
-- | A simple library for representing shapes.
module Shape where

-- Initial Goal:
--   * Two kinds of shapes: square and circle
--   * Two operations: area and perimeter
--
-- Later:
--   * add triangles
--   * add rectangles
--   * add diameter operation


type Radius = Float
type Length = Float
type Width = Float
data Shape = Circle Radius
           | Rectangle Length Width

  deriving (Eq,Show)

square :: Length -> Shape
square l =  Rectangle l l

-- | Compute the area of a shape.
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle l w) = l * w

-- | Compute the perimiter of a shape.
perimeter :: Shape -> Float
perimeter = undefined
