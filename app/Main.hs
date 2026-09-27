{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Raylib.Core
import Raylib.Core.Text
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Types
import Raylib.Core.Shapes 
import Raylib.Util.Math
import GHC.Float
import Data.Fixed

import Graph

startup :: IO WindowResources
startup = do
  setConfigFlags [Msaa4xHint]
  window <- initWindow 600 450 "SVPWM Visualiser"
  setTargetFPS 60
  return window

drawArrow :: Vector2 -> Float -> Float -> Float -> Color -> IO ()
drawArrow start size angle thick colour = 
  if size < 0 then
    drawArrow' start (-size) (pi + angle) thick colour
  else
    drawArrow' start size angle thick colour

drawArrow' :: Vector2 -> Float -> Float -> Float -> Color -> IO ()
drawArrow' start size angle thick colour = do
  drawLineEx start end lineThickness colour
  drawTriangle a b c colour
  where
    lineThickness = thick / 7
    triThickness = thick / 2
    end = arrowEnd start size angle
    a = start + vector2Rotate (Vector2 (max (size - thick) 0) $ -triThickness) angle
    b = start + vector2Rotate (Vector2 (max (size - thick) 0) triThickness) angle
    c = start + vector2Rotate (Vector2 size 0) angle

arrowEnd :: Vector2 -> Float -> Float -> Vector2
arrowEnd start size angle = start + vector2Rotate (Vector2 (size - 1) 0) angle

drawDial :: Vector2 -> Float -> Float -> Float -> Color -> IO ()
drawDial centre radius angle thick colour = do
  drawArrow centre radius angle thick colour
  drawPolyLinesEx centre 32 radius 0 2 colour

decimal :: Float -> Float 
decimal x = int2Float (ceiling x) - x

getAngle :: IO Float
getAngle = do
  t <- negateFloat . double2Float <$> getTime
  return $ 2 * pi * decimal (t / 10)

inputCircle :: [Float] -> IO ()
inputCircle [a, b, c] = do
  let centre = Vector2 120 225

  drawPolyLinesEx centre 40 100 0 2 black

  drawArrow centre a 0 10 green

  let aEnd = arrowEnd centre a 0

  drawArrow aEnd b (2/3 * pi) 10 red

  let bEnd = arrowEnd aEnd b (2/3 * pi)

  drawArrow bEnd c (4/3 * pi) 10 blue

  let cEnd = arrowEnd bEnd c (4/3 * pi)
      diff = cEnd - centre
      size = sqrt $ vector2'x diff ^ 2 + vector2'y diff ^ 2
      angle = -vector2LineAngle centre cEnd

  drawArrow centre size angle 10 black

inputCircle _ = return ()

trianglewave :: Float -> Float
trianglewave x' = -a + 4*a/p * abs (mod' (x - p/4) p - p/2)
  where
    x = x' - p/4
    a = 1
    p = 2 * pi

mainLoop :: WindowResources -> IO WindowResources
mainLoop window =
  drawing (
    do
      theta <- getAngle

      let instant = (\x -> x theta) <$> [a', b', c']

      clearBackground rayWhite
      drawGraph (do line <- lines; return $ line theta) (Vector2 300 120) 100 100
      drawGraph ([constructLine black 100 50 $ draw control 0] <> (magnitudeLines instant)) (Vector2 300 245) 100 100
      drawGraph [constructLine green 100 25 $ draw (pulse a' theta) theta] (Vector2 300 320) 100 50
      drawGraph [constructLine red 100 25 $ draw (pulse b' theta) theta] (Vector2 300 375) 100 50
      drawGraph [constructLine blue 100 25 $ draw (pulse c' theta) theta] (Vector2 300 430) 100 50
      inputCircle $ (66.6 *) <$> instant
  ) >> return window

  where
    range = [0, 0.05 .. 4 * pi]
    
    a' = sin 
    b' x = sin (2/3 * pi + x)
    c' x = sin (4/3 * pi + x)

    control _ = (*) 1.3 . trianglewave <$> range

    straightLine colour y =
      Line { points = [Vector2 0 y, Vector2 100 y], colour = colour }

    pulse :: (Float -> Float) -> Float -> p -> [Float]
    pulse f offset _ = (\y -> if (<) y $ f offset then 1 else 0) <$> control 0

    magnitudeLines :: [Float] -> [Line]
    magnitudeLines l = [straightLine green a'', straightLine red b'', straightLine blue c'']
      where [a'', b'', c''] = (\x -> 50 * x + 50) <$> l

    a offset = (\x -> sin (x + offset)) <$> range
    b offset = (\x -> sin (x + (2/3 * pi) + offset)) <$> range
    c offset = (\x -> sin (x + (4/3 * pi) + offset)) <$> range

    draw f offset = (1+) <$> f offset

    lines = [constructLine green 100 50 . draw a, constructLine red 100 50 . draw b, constructLine blue 100 50 . draw c]


shouldClose :: WindowResources -> IO Bool
shouldClose _ = windowShouldClose

teardown :: WindowResources -> IO ()
teardown = closeWindow . Just

$(raylibApplication 'startup 'mainLoop 'shouldClose 'teardown)