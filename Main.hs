{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear (V4(..))
import Linear (V2(..))
import Data.Vector.Storable (Vector (..))
import Control.Monad (unless)
import Snake
import GHC.Word (Word32)
import Foreign.C.Types
import System.Random

data SnakeGame = SnakeGame {snake :: Snake, timestamp :: Word32, fps :: Int, food :: (CInt, CInt)}

updateSnakeGame :: SnakeGame -> IO SnakeGame
updateSnakeGame s = do
  return (s)

initGame :: IO (SnakeGame)
initGame = do
    s <- newSnake
    s <- push s (0, 0)
    s <- push s (-16, 0)
    s <- setSnakeSize s 16
    t <- ticks
    fx <- randomIO :: IO CInt
    fy <- randomIO :: IO CInt
    snakeSize <- getSnakeSize s

    let x = ((mod fx ((800 - snakeSize) `div` snakeSize)) * snakeSize)
    let y = (mod fy ((600 - snakeSize) `div` snakeSize)) * snakeSize

    return (SnakeGame {snake = s, fps = 10, timestamp = t, food = (x, y)})

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Snakell V2" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  snakeGame <- initGame

  appLoop (renderer) (snakeGame)

appLoop :: Renderer -> SnakeGame -> IO ()
appLoop renderer s = do
  events <- pollEvents

  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  let eventIsDownArrowPress event = 
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeDown
          _ -> False
      downArrowPressed = any eventIsDownArrowPress events

  let eventIsUpArrowPress event = 
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeUp
          _ -> False
      upArrowPressed = any eventIsUpArrowPress events

  let eventIsLeftArrowPress event = 
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeLeft
          _ -> False
      leftArrowPressed = any eventIsLeftArrowPress events
  
  let eventIsRightArrowPress event = 
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeRight
          _ -> False
      rightArrowPressed = any eventIsRightArrowPress events

  d <- (getSnakeDir (snake s))
  s <- if downArrowPressed && d /= (0, -1)
    then do
      _s <- setSnakeDir (snake s) (0, 1)
      s <- updateSnakeGame (s {snake = _s})
      return s
    else
      do return (s)

  d <- (getSnakeDir (snake s))
  s <- if upArrowPressed && d /= (0, 1)
    then do
      _s <- setSnakeDir (snake s) (0, -1)
      s <- updateSnakeGame (s {snake = _s})
      return s
    else
      do return (s)

  d <- (getSnakeDir (snake s))
  s <- if leftArrowPressed &&  d /= (1, 0)
    then do
      _s <- setSnakeDir (snake s) (-1, 0)
      s <- updateSnakeGame (s {snake = _s})
      return s
    else
      do return (s)
  
  d <- (getSnakeDir (snake s))
  s <- if rightArrowPressed && d /= (-1, 0)
    then do
      _s <- setSnakeDir (snake s) (1, 0)
      s <- updateSnakeGame (s {snake = _s})
      return s
    else
      do return (s)

  t <- ticks

  let condition = (fromIntegral (t - (timestamp s)) >= (1000 `div` (fps s)))

  (renderer, s) <- if condition
    then do
      rendererDrawColor renderer $= V4 255 255 255 255
      clear renderer

      s <- updateSnakeGame (s {timestamp = t})

      let (foodx, foody) =food s
      let _snake = snake s

      size <- getSnakeSize _snake

      -- Update snake --
      _s <- moveSnake (snake s)
      s <- updateSnakeGame (s {snake = _s})

      ((x, y):xs) <- getSnakeBody (snake s)

      s <- if (foodx == x) && (foody == y)
        then do
          __s <- push (snake s) (x, y)
          snakeSize <- getSnakeSize __s
          fx <- randomIO :: IO CInt
          fy <- randomIO :: IO CInt

          b <- getSnakeBody __s

          s <- updateSnakeGame (s {fps = (fps s) + 1})
          s <- updateSnakeGame (s {snake = __s, food = ((mod fx ((800 - snakeSize) `div` snakeSize)) * snakeSize, (mod fy ((600 - snakeSize) `div` snakeSize)) * snakeSize)})
          return s
        else
          do return s

      snakeSize <- getSnakeSize (snake s)
      let hasCollidedWithItself ((a, b):xs) = or [a == c && b == d | (c, d) <- tail xs]
      let hasCollidedWithWall ((a, b):xs) = or [a < 0, a + snakeSize > 800, b < 0, b + snakeSize > 600]

      snakeBody <- getSnakeBody (snake s)

      let checkCollision snakeBody = (hasCollidedWithItself snakeBody || hasCollidedWithWall snakeBody)

      s <- if checkCollision snakeBody
        then do
          s <- initGame
          return s
        else
          do return (s)

      -- Draw food
      rendererDrawColor renderer $= V4 255 0 0 255
      fillRect renderer (Just (Rectangle (P (V2 foodx foody)) (V2 (size) (size))))

      -- Draw snake
      drawSnake renderer (snake s)

      present renderer
      return (renderer, s)
    else
      do
        return (renderer, s)

  unless qPressed (appLoop renderer s)