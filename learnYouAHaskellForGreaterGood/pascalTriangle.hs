import Data.List

pascalTriangle :: Int -> [[Int]]
pascalTriangle 1 = [[1]]
pascalTriangle 2 = [[1],[1,1]]
pascalTriangle n =
    let
        triangle = pascalTriangle (n - 1)
        prev = last triangle
    in
        triangle  ++ [[1] ++ map (\x -> prev !! x + prev !! (x - 1)) [1..n-2] ++ [1]]

main = do
    rows <- getLine

    let
        rowsInt = read rows::Int
        triangle = pascalTriangle rowsInt
        res = unlines (map (unwords . map show) triangle)

    putStrLn res