
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

data Shape = Circle Radius
           | Square Length
  deriving (Eq,Show)



-- | Compute the area of a shape.
area :: Shape -> Float
area = undefined

-- | Compute the perimiter of a shape.
perimeter :: Shape -> Float
perimeter = undefined
