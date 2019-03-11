import qualified Data.Set  as Set
import           System.IO

move :: Char -> (Int, Int) -> (Int, Int)
move direction (x, y) =
  case direction of
    '^' -> (x, y + 1)
    'v' -> (x, y - 1)
    '<' -> (x + 1, y)
    '>' -> (x - 1, y)
    _   -> error "Should not get here"

solve :: [Char] -> Set.Set (Int, Int)
solve directions =
  snd $
  foldl
    (\((x, y), visited) direction ->
       let newPosition = move direction (x, y)
        in (newPosition, Set.insert newPosition visited))
    ((0, 0), Set.empty)
    directions

split :: [a] -> ([a], [a])
split [] = ([], [])
split (h:t) =
  let (left, right) = split t
   in (h : right, left)

solve_second :: [Char] -> Set.Set (Int, Int)
solve_second directions = (Set.union (solve a) (solve b))
  where
    (a, b) = split directions

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn (show . Set.size $ solve contents)
  putStr (show . Set.size $ solve_second contents)
  hClose handle
