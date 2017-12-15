import System.Environment
import Data.List;

main :: IO ()
main = do
  (arg:_) <- getArgs
  print (map (baumSweet . fromDecimalToBinary) [0..(read arg)])

-- Thank you ehird: https://stackoverflow.com/a/9166342/6430775
fromDecimalToBinary :: Int -> [Int]
fromDecimalToBinary 0 = [0]
fromDecimalToBinary n = reverse (fromDecimalToBinary' n)

fromDecimalToBinary' :: Int -> [Int]
fromDecimalToBinary' 0 = []
fromDecimalToBinary' n = mod n 2 : fromDecimalToBinary' (div n 2)

isOddNumberOfZeros :: [Int] -> Bool
isOddNumberOfZeros number = case (length (filter (== 0) number)) `mod` 2 of
                              0 -> False
                              1 -> True

baumSweet :: [Int] -> Int
baumSweet [0] = 1
baumSweet digits = if any isOddNumberOfZeros (group digits)
                   then 0
                   else 1
