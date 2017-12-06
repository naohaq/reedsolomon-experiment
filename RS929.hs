{-# LANGUAGE TemplateHaskell, DataKinds, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.FiniteField hiding (toInteger)
import qualified Data.FiniteField as F (toInteger)

import ReedSolomon
import MyUtil

type F929 = $(primeField 929)

alpha :: F929
alpha = 3

f_alpha :: Int -> F929
f_alpha k = alpha ^ k

dec_pdf417 :: $(rsCode 40 22) F929
dec_pdf417 = RScode f_alpha

showF929 :: [F929] -> String
showF929 = joinStr " " . map (rjustify 3 . show)

dumpMsg :: [Int] -> [String]
dumpMsg [] = []
dumpMsg xs = (joinStr " " . map (rjustify 3 . show)) hs : dumpMsg ts
  where hs = take 8 xs
        ts = drop 8 xs

toF929 :: (Integral a) => a -> F929
toF929 = fromInteger . fromIntegral

fromF929 :: (Integral a) => F929 -> a
fromF929 = fromIntegral . F.toInteger

decode :: (ReedSolomon c F929) => c F929 -> [Int] -> ([Int], [(Int,F929)])
decode dec msg = (map fromF929 $ correctErrors errs mp, errs)
  where mp   = map toF929 msg
        par  = calcChecksum dec mp
        synd = calcSyndrome dec par
        errs = calcErrors dec synd

rws :: [Int]
rws = [897, 465, 237, 640, 111,  34, 672, 598,
       476,   0, 420, 329, 327,  17, 740, 136,
       806, 512, 918, 900,  57, 205, 129, 924,
       362, 242, 403, 601,  74, 702, 561,  35,
       413,  16, 542,  79,  34,  61, 351, 132]

main :: IO ()
main = do
  let dec = dec_pdf417
  putStrLn $ "RS Code: (" ++ show (block_N dec) ++ "," ++ show (block_K dec) ++ ")"
  putStrLn $ "Received message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws)
  let (rws_corr, errs) = decode dec rws
  putStrLn $ "Corrected message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws_corr)
  putStrLn $ "Errors: " ++ show errs
  putStrLn $ "Checksum : " ++ showF929 (calcChecksum dec $ map toF929 rws_corr)

-- EOF
