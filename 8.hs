module Main (main) where

main :: IO ()
main = do
  (t, []) <- parseTree . map read . words <$> getContents
  print $ part1 t
  print $ part2 t

parseTree :: [Int] -> (Tree, [Int])
parseTree (n:k:cs) = let (ts, cs') = parseTrees n cs
                     in (Tree ts $ take k cs', drop k cs')

parseTrees :: Int -> [Int] -> ([Tree], [Int])
parseTrees 0 cs = ([], cs)
parseTrees i cs = let (t, cs') = parseTree cs
                      (ts, cs'') = parseTrees (i-1) cs'
                  in (t:ts, cs'')

data Tree = Tree [Tree] [Int]
  deriving (Show)

part1 :: Tree -> Int
part1 (Tree ts meta) = sum meta + sum (map part1 ts)

part2 :: Tree -> Int
part2 (Tree [] meta) = sum meta
part2 (Tree ts meta) = sum $ map f meta
  where n = length ts
        f i | i > 0, i <= n = part2 $ ts !! (i-1)
            | otherwise = 0
