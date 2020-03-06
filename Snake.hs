module Snake (
    Snake,
    newSnake,
    getSnakeBody,
    setSnakeBody,
    getSnakeSize,
    setSnakeSize,
    setSnakeDir,
    push,
    pop,
    drawSnake,
    moveSnake
    ) where

import SDL
import Linear (V4(..))
import Linear (V2(..))
import Control.Monad (unless)
import Foreign.C.Types

data Snake = Snake {body :: [(CInt, CInt)], size :: CInt, dir :: (CInt, CInt)} deriving (Show)

newSnake :: IO Snake
newSnake = do return (Snake {body = [], size = 0, dir = (1, 0)})

getSnakeBody :: Snake -> IO [(CInt, CInt)]
getSnakeBody snake = do
    return (body snake)

setSnakeBody :: Snake -> [(CInt, CInt)] -> IO Snake
setSnakeBody snake b = do
    return (snake {body = b})

getSnakeSize :: Snake -> IO CInt
getSnakeSize snake = do
    return (size snake)

setSnakeSize :: Snake -> CInt -> IO Snake
setSnakeSize snake s = do
    return (snake {size = s})

getSnakeDir :: Snake -> IO (CInt, CInt)
getSnakeDir s = do
    return (dir s)

setSnakeDir :: Snake -> (CInt, CInt) -> IO Snake
setSnakeDir snake s = do
    return (snake {dir = s})

drawSnake :: Renderer -> Snake -> IO ()
drawSnake renderer s =
    do
        ((x, y):xs) <- getSnakeBody s
        snakeSize <- getSnakeSize s

        rendererDrawColor renderer $= V4 0 0 0 255
        fillRect renderer (Just (Rectangle (P (V2 x y)) (V2 (snakeSize) (snakeSize))))

        unless (length xs == 0) (drawSnake renderer (s {body = xs}))

moveSnake :: Snake -> IO Snake 
moveSnake s = do
    ((x, y):xs) <- getSnakeBody s
    (dx, dy) <- getSnakeDir s
    size <- getSnakeSize s
    s <- push s (x + size * dx, y + size * dy)
    s <- pop s

    return s

push :: Snake -> (CInt, CInt) -> IO Snake
push snake pos = do
    snakeBody <- getSnakeBody snake
    newSnakeBody <- (setSnakeBody snake ([pos] ++ snakeBody))

    return newSnakeBody

pop :: Snake -> IO Snake
pop snake = do
    snakeBody <- getSnakeBody snake
    s <- setSnakeBody snake (init snakeBody)

    return s