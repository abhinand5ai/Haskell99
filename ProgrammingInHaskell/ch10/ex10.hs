import GameOfLife (Board, glider)

putStr' :: String -> IO ()
putStr' s = sequence_ [putChar x | x <- s]
