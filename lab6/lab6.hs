{-# LANGUAGE RankNTypes #-}
{- Lab 6: PC-231 Simulator -}

module Main where

import Data.Array (listArray,Array,(!),(//),assocs,elems,ixmap)
import Data.Char (digitToInt, ord, chr)
import Control.Applicative
import Control.Lens
import Data.List (unfoldr)
import Text.Read (readMaybe)
import Numeric (readHex, showHex)
import System.IO

-- Typedefs --

type Register = Array Int Bool
type RAM = Array Int Register
type Registers = Array Int Register

data PC231 = PC231 Registers RAM

data Opcode
  = HALT
  | ZERO
  | SET
  | DATA
  | INC
  | SHIFT
  | ADD
  | SUB
  | AND
  | COPY
  | LOAD
  | STORE
  | READ
  | WRITE
  | JPIF
  | JUMP
  deriving (Show,Eq,Ord,Bounded,Enum)

data Device = DD | HD | AD deriving (Show,Eq,Enum)
data Condition = LZ | GZ | EZ | NZ deriving (Show,Eq,Enum)

instance Show PC231 where
  show = showPC

-- Init Values --

initRegister = listArray (0,11)  $ replicate 12 False
initRAM      = listArray (0,255) $ replicate 256 initRegister
initRegisters= listArray (0,15)  $ replicate 16  initRegister
initPC       = PC231 initRegisters initRAM

loadProg p   = ram %~ (//steps p) $ initPC
  where
    steps = zip [0..] . prog
    prog = map ((listArray (0,11) . int2bits . fst . head) . readHex )

pDouble = ["C00", "50E", "D00", "000"]
pAlpha  = ["305", "9EA", "35A", "9E0"
          ,"341", "DE2", "901", "7E1"
          ,"4E1", "E1C", "000"]



main = return ()

-- Sample Values --

testRAM = regAt 2.intval .~ 1 $ initRAM
lastRAM = regAt 255.intval .~ 17 $ testRAM

-- Register Lenses --

bits :: Lens' Register [Bool]
bits f r =
  (\l -> listArray (0,11) $ take 12 (l ++ repeat False))
    <$> f (elems r)

bits2int :: [Bool] -> Int
bits2int = foldr (\a b -> (b*2) + if a then 1 else 0) 0

int2bits :: Int -> [Bool]
int2bits n = (unfoldr $
  \int -> if int == 0 then Nothing else Just (int `mod` 2 == 1,int `div` 2)) n
  ++ repeat False

intval' :: Lens' [Bool] Int
intval' f bs =
  int2bits <$> f (bits2int bs)

intval :: Lens' Register Int
intval = bits.intval'

intval1s :: Lens' [Bool] Int
intval1s f bs =
  fmap fromOnes (f $ toOnes bs)
    where
      fromOnes :: Int -> [Bool]
      fromOnes int
        | int >= 0  = int2bits int
        | otherwise = map not . int2bits $ negate int

      toOnes :: [Bool] -> Int
      toOnes bs
        | last bs = negate . bits2int $ map not bs
        | otherwise = bits2int bs

intval2s :: Lens' [Bool] Int
intval2s f bs =
  fmap fromTwos (f $ toTwos bs)
    where
      fromTwos :: Int -> [Bool]
      fromTwos int
        | int >= 0  = int2bits int
        | otherwise = neg2 . int2bits $ negate int

      toTwos :: [Bool] -> Int
      toTwos bs
        | last bs = negate . bits2int $ neg2 bs
        | otherwise = bits2int bs

      neg2 :: [Bool] -> [Bool]
      neg2 [] = []
      neg2 (False:bs) = False : neg2 bs
      neg2 (True:bs) = True : map not bs

grp1,grp2,grp3 :: Lens' Register [Bool]
grp3 f r =
  (\grp -> set bits (take 4 (grp ++ repeat False) ++ drop 4 (view bits r)) r)
  <$> f (take 4 (view bits r))
grp2 f r =
  (\grp -> set bits (take 4 bit ++ take 4 (grp ++ repeat False) ++ drop 8 bit) r)
  <$> f (take 4 (drop 4 bit))
    where bit = view bits r
grp1 f r =
  (\grp -> set bits (take 8 bit ++ take 4 (grp ++ repeat False)) r)
  <$> f (drop 8 bit)
    where bit = view bits r

grp23 :: Lens' Register [Bool]
grp23 f r =
  (\grp -> set bits (take 8 grp ++ drop 8 bit) r)
  <$> f (view grp3 r ++ view grp2 r)
    where bit = view bits r

opcode :: Lens' Register Opcode
opcode f r =
  (\op -> set grp1 (int2bits $ fromEnum op) r)
  <$> f (toEnum $ bits2int (view grp1 r))

-- RAM Lenses --

regAt :: Int -> Lens' RAM Register
regAt n f ram =
  (\reg -> ram // [(n,reg)]) <$> f (ram ! n)

regAtValueOf :: Register -> Lens' RAM Register
regAtValueOf = regAt . bits2int . view bits

-- Free Register Lenses --

fRegAt :: Int -> Lens' Registers Register
fRegAt n f regs =
  (\reg -> regs // [(n,reg)]) <$> f (regs ! n)

f' :: Int -> Lens' Registers Register
f' n
  | n >= 0 && n <= 9 = fRegAt n
  | otherwise = error $ "Register R" ++ show n ++" does not exist"

j' :: Int -> Lens' Registers Register
j' n
  | n >= 0 && n <= 3 = fRegAt (n + 10)
  | otherwise = error $ "Register J" ++ show n ++" does not exist"

dr' :: Lens' Registers Register
dr' = fRegAt 14

pc' :: Lens' Registers Register
pc' = fRegAt 15

step' :: Registers -> Registers
step' = pc'.intval +~ 1

-- PC231 Lenses --

ram :: Lens' PC231 RAM
ram f (PC231 regs ram) =
  PC231 regs <$> f ram

regs :: Lens' PC231 Registers
regs f (PC231 regs ram) =
  flip PC231 ram <$> f regs

pcval :: PC231 -> Register
pcval pc = pc^.ram.regAtValueOf (pc^.regs.pc')

-- Program Logic --

stepwith :: (PC231 -> IO PC231) -> PC231 -> IO PC231
stepwith cb pc = case pcval pc ^. opcode of
  HALT  -> return  pc
  ZERO  -> pczero  pc cb reg2
  SET   -> pcset   pc cb reg2 (args^.grp3)
  DATA  -> pcdata  pc cb (args^.grp23)
  INC   -> pcinc   pc cb reg2 (args^.grp3.intval1s)
  SHIFT -> pcshift pc cb reg2 (args^.grp3.intval1s)
  ADD   -> pcadd   pc cb reg2 reg3
  SUB   -> pcsub   pc cb reg2 reg3
  AND   -> pcand   pc cb reg2 reg3
  COPY  -> pccopy  pc cb reg2 reg3
  LOAD  -> pcstore pc cb reg2 reg3
  STORE -> pcstore pc cb reg2 reg3
  READ  -> pcread  pc cb reg2 (args^.grp3.intval'.to toEnum)
  WRITE -> pcwrite pc cb reg2 (args^.grp3.intval'.to toEnum)
  JPIF  -> pcjpif  pc cb reg2
            (args^.grp3.to (drop 2).intval'.to toEnum)
            (args^.grp3.to (take 2).intval')
  JUMP  -> pcjump  pc cb (args^.grp23.intval')
  where
    args = pcval pc
    reg2 = args^.grp2.intval'
    reg3 = args^.grp3.intval'

run = stepwith run

stepthrough pc = do
  getChar
  print pc
  stepwith stepthrough pc

pczero :: PC231 -> (PC231 -> IO PC231) -> Int -> IO PC231
pczero pc cb reg = do
  let steppc = regs.fRegAt reg.intval .~ 0 $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcset :: PC231 -> (PC231 -> IO PC231) -> Int -> [Bool] -> IO PC231
pcset pc cb reg val = do
  let steppc = regs.fRegAt reg.grp3 .~ val $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcdata :: PC231 -> (PC231 -> IO PC231) -> [Bool] -> IO PC231
pcdata pc cb val = do
  let steppc = regs.dr'.grp23 .~ val $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcinc :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pcinc pc cb reg val = do
  let steppc = regs.fRegAt reg.bits.intval2s %~ (+ val) $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcshift :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pcshift pc cb reg val = do
    let steppc = regs.fRegAt reg.bits %~ shift val $ pc
    let newpc = regs %~ step' $ steppc
    cb newpc
  where
    shift 0 l = l
    shift n l
      | n < 0 = shift (n+1) $ False : init l
      | n > 0 = shift (n-1) $ tail l ++ [False]

pcadd :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pcadd pc cb r1 r2 = do
  let v1 = pc^.regs.fRegAt r1.bits.intval2s
  let steppc = regs.fRegAt r2.bits.intval2s %~ (+ v1) $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcsub :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pcsub pc cb r1 r2 = do
  let v1 = pc^.regs.fRegAt r1.bits.intval2s
  let steppc = regs.fRegAt r2.bits.intval2s %~ subtract v1 $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcand :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pcand pc cb r1 r2 = do
    let v1 = pc^.regs.fRegAt r1.bits
    let steppc = regs.fRegAt r2.bits %~ bitand v1 $ pc
    let newpc = regs %~ step' $ steppc
    cb newpc
  where
    bitand [] _ = []
    bitand _ [] = []
    bitand (a:as) (b:bs) = (a && b) : bitand as bs

pccopy :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pccopy pc cb r1 r2 = do
  let steppc = regs.fRegAt r2 .~ pc^.regs.fRegAt r1 $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcload :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pcload pc cb dest src = do
  let index = pc^.regs.fRegAt src.intval
  let v1 = pc^.ram.regAt index
  let steppc = regs.fRegAt dest .~ v1 $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcstore :: PC231 -> (PC231 -> IO PC231) -> Int -> Int -> IO PC231
pcstore pc cb src dest = do
  let index = pc^.regs.fRegAt dest.intval
  let v1 = pc^.regs.fRegAt src
  let steppc = ram.regAt index .~ v1 $ pc
  let newpc = regs %~ step' $ steppc
  cb newpc

pcread :: PC231 -> (PC231 -> IO PC231) -> Int -> Device -> IO PC231
pcread pc cb dest dev = case dev of
  DD -> do
    putStr "Please enter a number: "
    hFlush stdout
    d <- getLine
    case readMaybe d :: Maybe Int of
      Nothing  -> do
        putStrLn "Invalid number, try again."
        pcread pc cb dest dev
      Just val -> do
        let steppc = regs.regAt dest.bits.intval2s .~ val $ pc
        let newpc = regs %~ step' $ steppc
        cb newpc
  AD -> do
    putStrLn "Please enter a character:"
    c <- getChar
    if ord c > 4095
      then do
        putStrLn "Character too large to fit in memory!"
        pcread pc cb dest dev
      else do
        let steppc = regs.regAt dest.intval .~ ord c $ pc
        let newpc = regs %~ step' $ steppc
        cb newpc
  HD -> do
    putStrLn "Please enter a hex number:"
    h <- getLine
    case readHex h of
      [(n, "")] -> if n > 4095
        then do
          putStrLn "Number too large."
          pcread pc cb dest dev
        else do
          let steppc = regs.regAt dest.intval .~ n $ pc
          let newpc = regs %~ step' $ steppc
          cb newpc
      _ -> do
        putStrLn "Error parsing hex."
        pcread pc cb dest dev

pcwrite :: PC231 -> (PC231 -> IO PC231) -> Int -> Device -> IO PC231
pcwrite pc cb src dev =
  let val = case dev of
        DD -> pc^.regs.regAt src.bits.intval2s.to show
        AD -> [pc^.regs.regAt src.intval.to chr]
        HD -> pc^.regs.regAt src.intval.to showHex $ ""
  in do
    putStr $ "Output from " ++ show dev ++ ": "
    putStrLn val
    let newpc = regs %~ step' $ pc
    cb newpc

pcjpif :: PC231 -> (PC231 -> IO PC231) -> Int -> Condition -> Int -> IO PC231
pcjpif pc cb reg cond jn = do
  let val = pc^.regs.regAt reg.bits.intval2s
  let exp = case cond of
        LZ -> val < 0
        GZ -> val > 0
        EZ -> val == 0
        NZ -> val /= 0
  let newpc = if exp
        then regs.pc' .~ (pc^.regs.j' jn) $ pc
        else regs %~ step' $ pc
  cb newpc

pcjump :: PC231 -> (PC231 -> IO PC231) -> Int -> IO PC231
pcjump pc cb val = do
  let newpc = (regs.pc'.grp23.intval' .~ val) . (regs.pc'.intval .~ 0)  $ pc
  cb newpc

-- Misc --

printR :: Register -> String
printR r = unwords $ map (bitsFrom r) [grp1,grp2,grp3]
  where
    bitsFrom r grp = reverse .
      fmap (\b -> if b then '1' else '0') . view grp $ r

printRAM :: RAM -> String
printRAM ram = unlines $ header :
  reverse (map printR' (dropWhile isZero (reverse $ assocs ram))) ++
  [zeroprompt | isZero $ last (assocs ram)] ++ [bottom]
  where
    isZero (n,r) = view intval r == 0
    header     = "╭── RAM ─────────────╮"
    zeroprompt = "│  (rest are zero)   │"
    bottom     = "╰────────────────────╯"
    printR' (n,r) = "│ " ++ padded L 4 (show n) ++ printR r ++ " │"

printRegs :: Registers -> String
printRegs regs = unlines $ header :
  pregs (0,9) ++ [sep] ++ pregs (10,13) ++ [sep] ++ pregs (14,15) ++ [bottom]
  where
    pregs range = map printR' (assocs $ ixmap range id regs)
    header = "╭── REGISTERS ───────╮"
    sep    = "├────────────────────┤"
    bottom = "╰────────────────────╯"
    printR' (n,r) = "│ " ++ regnum n ++ printR r ++ " │"
    regnum n
      | n `elem` [ 0..9 ] = padded L 4 $ 'R' : show n
      | n `elem` [10..13] = padded L 4 $ 'J' : show (n-10)
      | n == 14           = padded L 4 "DR"
      | otherwise         = padded L 4 "PC"

showPC :: PC231 -> String
showPC pc = columns L Top 24 [printRAM $ pc^.ram, printRegs $ pc^.regs]

data VAlign = Top | Bottom
data HAlign = L | R | M

padded :: HAlign -> Int -> String -> String
padded L k n = n ++ replicate (k - length n) ' '
padded R k n = replicate (k - length n) ' ' ++ n
padded M k n = replicate (floor (fromIntegral (k - length n) / 2)) ' '
               ++ n ++
               replicate (ceiling (fromIntegral (k - length n) / 2)) ' '


columns :: HAlign -> VAlign -> Int -> [String] -> String
columns h va p cs = unlines . map concat . getZipList $
  traverse (ZipList . map (padded h p) . extend . lines) cs
  where
    extend c = case va of
      Top    -> c ++ replicate (totallen - length c) (replicate p ' ')
      Bottom -> replicate (totallen - length c) (replicate p ' ') ++ c
    totallen = maximum $ map (length . lines) cs


-- Program IO --

