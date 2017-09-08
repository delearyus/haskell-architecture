{- Lab 4 : Hidden circuit simulator -}

module Main where

import qualified Data.Map.Strict as Map
import Data.Char (toLower, toUpper)
import Data.Maybe (isJust)
import qualified Control.Applicative as App
import System.IO
import Control.Monad (void)
import System.Console.ANSI (clearScreen, setCursorPosition)

type ID = String

data Circuit = Circuit (Map.Map ID Gate) [ID] [ID] deriving Show
-- The Map contains all gates, while the two lists contain lights and
-- switches, respectively.

data Gate
  = Switch Bool
  | Const Bool
  | Light ID
  | Not   ID
  | Nand  ID ID
  | And   ID ID
  | Or    ID ID
  | Nor   ID ID
  deriving (Show,Eq)

readLineMap :: String -> Maybe (ID, Gate)
readLineMap str
  | head args == "switch" = if length args == 2
      then Just (args !! 1, Switch False)
      else Nothing
  | head args == "light" = if length args == 3
      then Just (args !! 1, Light (args !! 2))
      else Nothing
  | head args == "not"   = if length args == 3
      then Just (args !! 1, Not   (args !! 2))
      else Nothing
  | head args == "nand"  = if length args == 4
      then Just (args !! 1, Nand  (args !! 2) (args !! 3))
      else Nothing
  | head args == "and"   = if length args == 4
      then Just (args !! 1, And   (args !! 2) (args !! 3))
      else Nothing
  | head args == "or"    = if length args == 4
      then Just (args !! 1, Or    (args !! 2) (args !! 3))
      else Nothing
  | head args == "nor"   = if length args == 4
      then Just (args !! 1, Nor   (args !! 2) (args !! 3))
      else Nothing
  | otherwise = Nothing
  where args = words $ map toLower str

scanfor :: String -> String -> [ID]
scanfor name str
  | head args == name = [args !! 1]
  | otherwise = []
  where args = words $ map toLower str

listOf :: String -> String -> Maybe [ID]
listOf str = Just . concatMap (scanfor str) . filter (not . null) . lines

buildMap = fmap Map.fromList . mapM readLineMap . filter (not . null) . lines

readCircuit str = Circuit
  <$> buildMap str
  <*> listOf "light" str
  <*> listOf "switch" str

eval :: Circuit -> ID -> Bool
eval c@(Circuit m l s) g = case Map.lookup g m of
  Just (Switch b) -> b
  Just (Const  b) -> b
  Just (Light id) -> eval c id
  Just (Not   id) -> not $ eval c id
  Just (And  ida idb) -> eval c ida && eval c idb
  Just (Or   ida idb) -> eval c ida || eval c idb
  Just (Nand ida idb) -> not $ eval c ida && eval c idb
  Just (Nor  ida idb) -> not $ eval c ida || eval c idb
  Nothing -> error $ "Error: gate [" ++ g ++ "] " ++
                     "is required as input but does not exist"

flipSwitch :: Circuit -> ID -> Circuit
flipSwitch c@(Circuit m l s) id = if id `elem` s
  then Circuit (Map.adjust switch id m) l s
  else c
  where
    switch :: Gate -> Gate
    switch g = case g of Switch b -> Switch $ not b

prettyPrint :: Circuit -> String
prettyPrint c@(Circuit m l s) = switchBlock ++ blackBox ++ lightBlock
  where
    switchBlock = (unlines . App.getZipList
      . fmap concat . traverse drawSwitch) s
    lightBlock = (unlines . App.getZipList
      . fmap concat . traverse drawLight) l
    drawSwitch id = case m Map.! id of
      Switch b -> App.ZipList
        ["┌"  ++ replicate 7 '─'            ++ "┐"
        ,"│ " ++ take 2 (map toUpper id)    ++ " "
              ++ (if b then "⚫" else "⚪") ++ " │"
        ,"└"  ++ replicate 3 '─'            ++ "╥"
              ++ replicate 3 '─'            ++ "┘"]
    blackBox =
      (if length s == 1
        then "    ║\n"
        else
          "    ╠════" ++
          concat ( replicate (length s - 2)
          "════╩════") ++
          "════╝\n") ++
      " ┌──╨──┐\n" ++
      " │  ?  │\n" ++
      " └──╥──┘\n" ++
      (if length l == 1 then "    ║\n"
        else
          "    ╠════" ++
          concat ( replicate (length l - 2)
          "════╦════") ++
          "════╗\n")
    drawLight id = case m Map.! id of
      Light _ -> App.ZipList
        ["┌"  ++ replicate 3 '─'            ++ "╨"
              ++ replicate 3 '─'            ++ "┐"
        ,"│ " ++ take 2 (map toUpper id)    ++ " "
              ++ (if eval c id then "⚫" else "⚪") ++ " │"
        ,"└"  ++ replicate 7 '─'            ++ "┘"]

displayPrompt c = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ prettyPrint c
  putStr "Switch to Flip: "
  hFlush stdout
  id <- getLine
  displayPrompt (flipSwitch c (map toLower id))
  return ()

main = do
  putStrLn "Please enter the name of the circuit file: "
  fname <- getLine
  fileContents <- readFile fname
  case readCircuit fileContents of
    Nothing -> error $ "Cannot parse circuit, " ++
      "please fix your circuit and try again"
    Just c -> displayPrompt c
  return ()
