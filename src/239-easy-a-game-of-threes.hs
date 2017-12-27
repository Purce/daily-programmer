import System.Environment

main :: IO ()
main = do
  (x:_) <- getArgs
  print $ f (read x :: Int)

f :: Int -> [(Int, Int)]
f 1 = [(1, 0)]
f n
  | mod n 3 == 0 = (n, 0) : f (div n 3)
  | mod (n + 1) 3 == 0 = (n, 1) : f (div (n + 1) 3)
  | mod (n - 1) 3 == 0 = (n, (-1)) : f (div (n - 1) 3)
  
