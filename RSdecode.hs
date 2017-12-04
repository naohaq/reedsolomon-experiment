{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.GF2Extension
import Data.GF256
import qualified Data.Word as W

import ReedSolomon
import MyUtil

type F256 = GF256 PP301

type Decoder = $(rsDecoder 40 18) F256

dec_ecc200 :: Decoder
dec_ecc200 = RSdecoder pow2

toWord8 :: F256 -> W.Word8
toWord8 x = (fromIntegral . toInt) x

showHex :: [F256] -> String
showHex = joinStr " " . map (fromW8toHex . toWord8)

dumpMsg :: [W.Word8] -> [String]
dumpMsg [] = []
dumpMsg xs = (joinStr " " . map fromW8toHex) hs : dumpMsg ts
  where hs = take 8 xs
        ts = drop 8 xs

-- Received message with burst error.
rws :: [W.Word8]
rws = [0x8e, 0x32, 0x2e, 0xbe, 0x92, 0x35, 0x2e, 0x83,
       0x21, 0x73, 0x66, 0x77, 0x3f, 0xff, 0x21, 0x92,
       0x8a, 0x4c, 0x40, 0x00, 0x06, 0x37, 0x24, 0xa3,
       0xa7, 0x8e, 0xf0, 0x7b, 0xea, 0x38, 0xe7, 0x57,
       0xcd, 0x1d, 0x7a, 0x7e, 0xd8, 0xb2, 0x24, 0xe9]

main :: IO ()
main = do
  let code_N  = block_N dec_ecc200
  let code_2t = block_K dec_ecc200
  putStrLn $ "RS Code: (" ++ show code_N ++ "," ++ show (code_N - code_2t) ++ ")"
  putStrLn $ "Received message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws)
  let csum = calcChecksum dec_ecc200 rws
  putStrLn $ "Checksum : " ++ showHex csum
  let synd = calcSyndrome dec_ecc200 csum
  putStrLn $ "Syndromes: " ++ showHex synd
  let sigma_r = errLocator dec_ecc200 synd
  putStrLn $ "Error locator: " ++ showHex sigma_r
  let locs = solveErrLocations dec_ecc200 sigma_r
  let locs_r = [code_N - 1 - k | k <- reverse locs]
  putStrLn $ "Error locations: " ++ show locs_r
  let mtx = errMatrix dec_ecc200 locs synd
  putStrLn "Error matrix:"
  mapM_ (putStrLn . ("   [ " ++) . (++ " ]") . showHex) mtx
  let evs = solveErrMatrix mtx
  let evs_r = reverse evs
  putStrLn $ "Error values: " ++ showHex evs_r
  let errs = zip locs_r evs_r
  let rws_corr = map toWord8 $ correctErrors errs $ map (fromInteger . fromIntegral) rws
  putStrLn $ "Corrected message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws_corr)
  putStrLn $ "Checksum : " ++ showHex (calcChecksum dec_ecc200 rws_corr)

-- EOF
