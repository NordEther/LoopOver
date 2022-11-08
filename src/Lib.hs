module Lib
    ( formatGrid
    , insertAt
    , output
    , rotate
    , remove
    , grid
    , grid5
    ) where


formatGrid :: [String] -> String
formatGrid = unlines

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

output :: Show a => [a] -> IO ()
output = putStrLn . unlines . map show

rotate xs n = rot n xs
    where rot 0 = id
          rot 1 = \xs -> case xs of [] -> []; xs -> tail xs ++ [head xs]
          rot (-1) = \xs -> case xs of [] -> []; xs -> (last xs):init xs
          rot n
              | n > 0 = (rot (n-1)).(rot 1)
              | n < 0 = (rot (n+1)).(rot (-1))

remove n xs = let (as, bs) = splitAt n xs in as ++ tail bs

grid x = ["1 " ++ x !! 0] ++ ["2 " ++ x !! 1] ++ ["3 " ++ x !! 2] ++ ["  456"]
grid5 x = ["1 " ++ x !! 0] ++ ["2 " ++ x !! 1] ++ ["3 " ++ x !! 2] ++ ["4 " ++ x !! 3] ++ ["5 " ++ x !! 4]++ ["  678910"]