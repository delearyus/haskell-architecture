{- Lab 2: Binary addition with two's complement -}

module Main where

import Control.Lens
import System.IO (hFlush, stdout)

data ByteN = ByteN Int [Bool] deriving Eq
-- Data is represented in reverse order from normal left to right reading

instance Show ByteN where
  show b@(ByteN n bs) = '[' : show n ++ "] " ++ 
    showbits (reverse (view bits b))
    where
      showbits [] = ""
      showbits (True:bs) = '1' : showbits bs
      showbits (False:bs) = '0' : showbits bs

baseN :: Int -> ByteN
baseN n = ByteN n $ replicate n False

base8 :: ByteN
base8 = baseN 8

bits :: Lens' ByteN [Bool]
bits f (ByteN n bs) =
  fmap (\l -> ByteN n $ take n (l ++ repeat False))
    (f $ take n (bs ++ repeat False))



intval :: Lens' ByteN Int
intval f b@(ByteN n bs) =
  fmap (\int -> set bits (fromTwos int) b) (f (toTwos (view bits b)))
    where
      fromTwos :: Int -> [Bool]
      fromTwos int
        | int >= 0  = toByte int
        | otherwise = neg2 . toByte $ negate int

      toTwos :: [Bool] -> Int
      toTwos bs
        | last bs = negate . fromByte $ neg2 bs
        | otherwise = fromByte bs

      neg2 :: [Bool] -> [Bool]
      neg2 [] = []
      neg2 (False:bs) = False : neg2 bs
      neg2 (True:bs) = True : map not bs

      fromByte :: [Bool] -> Int
      toByte :: Int -> [Bool]
      -- fromByte and toByte act on a list of bools of arbitrary length
      -- and thus do not do any truncating or overflowing, additionally they
      -- only work on unsigned values
      fromByte [] = 0
      fromByte (b:bs) = (if b then 1 else 0) + 2 * fromByte bs

      toByte 0 = repeat False
      toByte n = (n `mod` 2 == 1) : toByte (n `div` 2)

add :: ByteN -> ByteN -> ByteN
-- Bytes need to be the same size or else it will work strangely!
-- specifically, it will truncate the longer byte but put it back into ba
-- May change it later to expand the size of the smaller bite, idk
add ba bb = let [as,bs] = map (view bits) [ba,bb] in
  set bits (add' False as bs) ba
  where
    add' _ [] _ = []
    add' _ _ [] = []
    add' False (a:as) (b:bs) = (a /= b) : add' (a && b) as bs
    add' True (a:as) (b:bs) =  (a == b) : add' (a || b) as bs


detectOverflow :: ByteN -> ByteN -> ByteN -> IO ()
detectOverflow a b r = 
  if (positive a && positive b && negative r)
  || (negative a && negative b && positive r)
  then putStrLn "Overflow: yes"
  else putStrLn "Overflow: no"
    where
      positive,negative :: ByteN -> Bool
      positive b@(ByteN n bs) = view bits b !! (n-1)

      negative = not . positive

main :: IO ()
main = do
  putStr "Byte size: "
  hFlush stdout
  ns <- getLine
  let n = read ns :: Int
  putStr "Number one: "
  hFlush stdout
  num1 <- getLine
  putStr "Number two: "
  hFlush stdout
  num2 <- getLine
  let (a,b) = (read num1,read num2) :: (Int, Int)
  let (ba, bb) = (set intval a (baseN n), set intval b (baseN n))
  putStr "Sum: "
  let sum = add ba bb
  print $ view intval sum
  putStr "Binary: "
  print sum
  detectOverflow ba bb sum
  return ()
