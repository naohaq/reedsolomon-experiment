{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.GF2Extension
import Data.GF256
import qualified Data.Word as W

import ReedSolomon
import MyUtil

type F256 = GF256 PP301

dec_weak :: $(rsCode 30 22) F256
dec_weak = RScode pow2

dec_ecc200 :: $(rsCode 40 22) F256
dec_ecc200 = RScode pow2

showHex :: [F256] -> String
showHex = joinStr " " . map (fromW8toHex . toWord8)

dumpMsg :: [W.Word8] -> [String]
dumpMsg [] = []
dumpMsg xs = (joinStr " " . map fromW8toHex) hs : dumpMsg ts
  where hs = take 8 xs
        ts = drop 8 xs

decode :: (ReedSolomon c F256) => c F256 -> [W.Word8] -> Maybe ([W.Word8], [(Int,W.Word8)])
decode dec msg | all (==0) par    = Just (msg, [])
               | length errs < 1  = Nothing
               | otherwise        = Just (map toWord8 $ correctErrors errs mp, errs')
  where mp   = map fromWord8 msg
        par  = calcChecksum dec mp
        synd = calcSyndrome dec par
        errs = calcErrors dec synd
        errs' = [(i, toWord8 v) | (i,v) <- errs]

-- Received message with burst error.
rws :: [W.Word8]
rws = [0x8e, 0x32, 0x2e, 0xbe, 0x92, 0x35, 0x2e, 0x83,
       0x21, 0x73, 0x66, 0x77, 0x3f, 0xff, 0x21, 0x92,
       0x8a, 0x4c, 0x40, 0x00, 0x06, 0x37, 0x24, 0xa3,
       0xa7, 0x8e, 0xf0, 0x7b, 0xea, 0x38, 0xe7, 0x57,
       0xcd, 0x1d, 0x7a, 0x7e, 0xd8, 0xb2, 0x24, 0xe9]

-- Message with too many errors.
rws_fail :: [W.Word8]
rws_fail = [0x8e, 0x32, 0x2e, 0xbe, 0x92, 0x35, 0x2e, 0x83,
            0x21, 0x73, 0x66, 0x77, 0x3f, 0xff, 0x21, 0x92,
            0x8a, 0x4c, 0x40, 0x00, 0x06, 0x37, 0x4c, 0x27,
            0xc2, 0x56, 0xc9, 0x03, 0x6f, 0x3f]

main :: IO ()
main = do
  let dec = dec_ecc200
  putStrLn $ "RS Code: (" ++ show (block_N dec) ++ "," ++ show (block_K dec) ++ ")"
  putStrLn $ "Received message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws)
  let Just (rws_corr, errs) = decode dec rws
  putStrLn $ "Corrected message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws_corr)
  putStrLn $ "Errors: " ++ joinStr ", " [show i ++ ": " ++ fromW8toHex v | (i,v) <- errs]
  putStrLn $ "Checksum : " ++ showHex (calcChecksum dec (map fromWord8 rws_corr))

-- EOF
