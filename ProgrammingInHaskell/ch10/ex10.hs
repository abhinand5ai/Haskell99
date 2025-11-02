import Nim (Board, putRow)

putStr' :: String -> IO ()
putStr' s = sequence_ [putChar x | x <- s]

putBoardSeq :: Board -> IO ()
putBoardSeq b = sequence_ [putRow r v | (r, v) <- zip [1 ..] b]

putBoardRec :: Board -> IO ()
putBoardRec b = do
  putB 1 b
  where
    putB :: Int -> Board -> IO ()
    putB r (x:xs) =
      do
        putRow r x
        putB (r + 1) xs
    putB _ [] = return ()