import System.Environment

main :: IO ()
main = do
  (arg:xs) <- getArgs
  let numbers = [0..(read arg)]
  let bin = map fromDecimalToBinary numbers
  let res = map baumSweet bin
  print res

-- Thank you ehird: https://stackoverflow.com/a/9166342/6430775
fromDecimalToBinary :: Int -> [Int]
fromDecimalToBinary 0 = [0]
fromDecimalToBinary n = reverse (fromDecimalToBinary' n)

fromDecimalToBinary' :: Int -> [Int]
fromDecimalToBinary' 0 = []
fromDecimalToBinary' n = mod n 2 : fromDecimalToBinary' (div n 2)
                       
baumSweet :: [Int] -> Int
baumSweet [0] = 1
baumSweet number = baumSweet' number 0

baumSweet' :: [Int] -> Int -> Int
baumSweet' [] i =
  case mod i 2 of
    0 -> 1
    1 -> 0
baumSweet' (x:xs) i =
  case x of
    0 -> baumSweet' xs (i + 1)
    1 -> case mod i 2 of
           0 -> baumSweet' xs 0
           1 -> 0
