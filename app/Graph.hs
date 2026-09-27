{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Graph where

import Raylib.Core.Shapes
import Raylib.Types
import Raylib.Util.Colors
import GHC.Float

data Line = Line {
  points :: [Vector2],
  colour :: Color
}

addPoint :: Line -> Vector2 -> Line
addPoint (Line { points = xs, colour = colour }) x = 
  if length xs >= 64 then
    Line {points = (tail ys), colour = colour}
  else
    Line {points = ys, colour = colour}
  
  where
    ys = xs <> [x]

drawLine' :: Vector2 -> Float -> Line -> IO ()
drawLine' corner height Line {points = points, colour = colour} =
  drawSplineLinear (((\p -> (Vector2 ((vector2'x corner) + (vector2'x p))) $  (vector2'y p) + (vector2'y corner) - height)) <$> points) 2 colour

drawLines :: Vector2 -> Float -> [Line] -> IO ()
drawLines corner height (line:lines) = do
  drawLine' corner height line
  drawLines corner height lines
drawLines corner height [line] = do
  drawLine' corner height line
drawLines _ height [] = return ()

constructLine :: Color -> Float -> Float -> [Float] -> Line
constructLine colour width yscale points = Line {points = points_vector, colour = colour}
  where
    (_, points_vector) = foldr (\y (n, l) -> (n + 1, l <> [Vector2 (n * increment) $ y * yscale])) (0, []) points
    increment = (/) width (int2Float $ length points - 1) :: Float

drawGraph :: [Line] -> Vector2 -> Float -> Float -> IO ()
drawGraph lines corner width height = do
  drawLineEx corner (Vector2 (vector2'x corner) ((vector2'y corner) - height)) 2 lightGray
  drawLineEx corner (Vector2 ((vector2'x corner) + width) $ vector2'y corner) 2 lightGray
  drawLines corner height lines