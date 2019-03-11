import           System.IO

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

nTimes :: (Int -> Int) -> Int -> Int -> Int
nTimes f acc 0 = acc
nTimes f acc n = nTimes f (f acc) (n - 1)

times :: Int -> Int -> Int
times x y
  | x < y =
    if even y
      then (y - 1) * (y - 1) + x - 2
      else (y * y) - x
  | otherwise =
    if even x
      then (x * x) - y
      else (x - 1) * (x - 1) + y - 2

solve :: Int -> Int -> Int
solve x y = times x y |> (flip mod) 33726 |> compute

compute :: Int -> Int
compute n = nTimes (\x -> mod (x * 85643) 8546517) 20180323 n

main = solve 11023491 43120341252 |> show |> putStrLn
