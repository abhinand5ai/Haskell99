import GameOfLife qualified as G (Board, glider)
import Nim qualified as N (Board)

putStr' :: String -> IO ()
putStr' s = sequence_ [putChar x | x <- s]
