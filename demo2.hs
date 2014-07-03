module Main where

import Data.Array.IO
import Unsafe.Coerce
import GHC.Prim

data Op
  = OpAdd -- addr1 addr2 addr_result
  | OpNegative -- addr addr_result
  | OpPrint -- addr
  | OpJump -- addr_next
  | OpEnd

code =
  [ unsafeCoerce OpJump -- 0
  , unsafeCoerce (4 :: Int) -- 1
  , unsafeCoerce (16 :: Int) -- 2
  , unsafeCoerce (2 :: Int) -- 3
  , unsafeCoerce OpNegative -- 4 *
  , unsafeCoerce (3 :: Int) -- 5
  , unsafeCoerce (3 :: Int) -- 6
  , unsafeCoerce OpAdd -- 7
  , unsafeCoerce (2 :: Int) -- 8
  , unsafeCoerce (3 :: Int) -- 9
  , unsafeCoerce (1 :: Int) -- 10
  , unsafeCoerce OpJump -- 11
  , unsafeCoerce (0 :: Int) -- 12
  , unsafeCoerce (8 :: Int) -- 13
  , unsafeCoerce OpPrint -- 14 *
  , unsafeCoerce (1 :: Int) -- 15
  , unsafeCoerce OpAdd -- 16
  , unsafeCoerce (1 :: Int) -- 17
  , unsafeCoerce (13 :: Int) -- 18
  , unsafeCoerce (1 :: Int) -- 19
  , unsafeCoerce OpJump -- 20
  , unsafeCoerce (0 :: Int) -- 21
  , unsafeCoerce OpEnd -- 22 *
  ]

process :: IOArray Int Any -> IO ()
process memory = go 0 where
  r addr = fmap unsafeCoerce $ readArray memory addr
  w value addr = writeArray memory addr (unsafeCoerce value)
  go ip = do
    op <- r ip
    case op of
      OpAdd -> do
        arg1 <- r (ip+1) >>= r
        arg2 <- r (ip+2) >>= r
        r (ip+3) >>= w (arg1 + arg2 :: Double)
        go (ip+4)
      OpNegative -> do
        arg <- r (ip+1) >>= r
        r (ip+2) >>= w (-arg)
        go (ip+3)
      OpPrint -> do
        arg <- r (ip+1) >>= r
        putStrLn $ show (arg :: Int)
        go (ip+2)
      OpJump -> do
        newIP <- r (ip+1)
        go newIP
      OpEnd ->
        return ()

main = do
  memory <- newListArray (0, length code - 1) code
  process memory
