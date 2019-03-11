import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import           System.IO

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

bumpCount :: Char -> Map.Map Char Integer -> Map.Map Char Integer
bumpCount char map =
  case (Map.lookup char map) of
    Just count -> Map.insert char (count + 1) map
    Nothing    -> Map.insert char 1 map

solve :: [[Char]] -> [Char]
solve lines =
  foldl
    (\maps line -> zip line maps |> map (\(char, map) -> bumpCount char map))
    maps
    lines |>
  map Map.toList |>
  map
    (List.maximumBy (\(char1, count1) (char2, count2) -> compare count1 count2)) |>
  map fst
  where
    colCount = length (head lines)
    maps = take colCount (repeat Map.empty)

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn (show . solve $ lines contents)
  hClose handle
