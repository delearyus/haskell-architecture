{- Lab 1: Binary numerals -}

module Lab1 where

import Data.Char (ord,chr)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

cases :: String -> (String,String)
-- converts "String" to ("STRING","string")

cases str = (ucase str,dcase str)
  where
    ucase [] = []
    ucase (s:str)
      | ord s `elem` [ord 'a' .. ord 'z'] =
        chr (ord s + ord 'A' - ord 'a') : ucase str
      | otherwise = s : ucase str

    dcase [] = []
    dcase (s:str)
      | ord s `elem` [ord 'A' .. ord 'Z'] =
        chr (ord s + ord 'a' - ord 'A') : dcase str
      | otherwise = s : dcase str


type Base = Int

bases :: Base -> Base -> String -> (String, Int)
-- base :: Input Base ->
--         Output Base ->
--         Input Numeral ->
--         (Output Numeral, Integer value)
--
-- ex: bases 16 2 "FF" = ("11111111",255)

bases bi bo strin = (reverse $ to bo numval, numval)
  where
    numval = from bi (reverse strin)

    from :: Base -> String -> Int
    from b "" = 0
    from b (d:digs)
      | d `elem` digits b =
        fromJust (elemIndex d (digits b)) + (b * from bi digs)
      | otherwise = error $ "Error: " ++ [d] ++ " is not a digit!"

    to :: Base -> Int -> String
    to _ 0   = ""
    to b int
      | b <= 64 = (digits b !! (int `mod` b)) : to b (int `div` b)
      | otherwise = error "Unsupported Base!"

    digits b
      | b <= 36 = ['0'..'9'] ++ ['A'..'Z']
      | otherwise = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/"
        -- conformity to base64 standard, which puts 0-9 after A-Z and a-z, for
        -- whatever reason

