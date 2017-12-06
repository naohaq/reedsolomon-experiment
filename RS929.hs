{-# LANGUAGE DataKinds, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.FiniteField hiding (toInteger)
import qualified Data.FiniteField as F (toInteger)

import ReedSolomon
import MyUtil

type F929 = $(primeField 929)

type Decoder = $(rsCode 40 22) F929

alpha :: F929
alpha = 3

f_alpha :: Int -> F929
f_alpha k = alpha ^ k

dec_pdf417 :: Decoder
dec_pdf417 = RScode f_alpha

showF929 :: [F929] -> String
showF929 = joinStr " " . map (rjustify 3 . show)

dumpMsg :: [Int] -> [String]
dumpMsg [] = []
dumpMsg xs = (joinStr " " . map (rjustify 3 . show)) hs : dumpMsg ts
  where hs = take 8 xs
        ts = drop 8 xs

rws :: [Int]
rws = [897, 465, 237, 640, 111,  34, 672, 598,
       476,   0, 420, 329, 327,  17, 740, 136,
       806, 512, 918, 900,  57, 205, 129, 924,
       362, 242, 403, 601,  74, 702, 561,  35,
       413,  16, 542,  79,  34,  61, 351, 132]

toF929 :: (Integral a) => a -> F929
toF929 = fromInteger . fromIntegral

fromF929 :: (Num a) => F929 -> a
fromF929 = fromIntegral . F.toInteger

main :: IO ()
main = do
  let dec = dec_pdf417
  putStrLn $ "RS Code: (" ++ show (block_N dec) ++ "," ++ show (block_K dec) ++ ")"
  putStrLn $ "Received message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws)
  let csum = calcChecksum dec (map toF929 rws)
  putStrLn $ "Checksum : " ++ showF929 csum
  let synd = calcSyndrome dec csum
  putStrLn $ "Syndromes: " ++ showF929 synd
  let sigma_r = calcErrLocator dec synd
  putStrLn $ "Error locator: " ++ showF929 sigma_r
  let locs = solveErrLocations dec sigma_r
  let locs_r = [block_N dec - 1 - k | k <- reverse locs]
  putStrLn $ "Error locations: " ++ show locs_r
  let mtx = calcErrMatrix dec locs synd
  putStrLn "Error matrix:"
  mapM_ (putStrLn . ("   [ " ++) . (++ " ]") . showF929) mtx
  let evs = solveErrMatrix mtx
  let evs_r = reverse evs
  putStrLn $ "Error values: " ++ showF929 evs_r
  let rws_corr :: [Int]
      rws_corr = map fromF929 $ correctErrors (zip locs_r evs_r) $ map toF929 rws
  putStrLn $ "Corrected message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws_corr)
  putStrLn $ "Checksum : " ++ showF929 (calcChecksum dec (map toF929 rws_corr))

-- EOF
