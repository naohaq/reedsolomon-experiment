{-# LANGUAGE DataKinds, TemplateHaskell #-}
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

gp_pdf417 :: [F929]
gp_pdf417 = gen_poly f_alpha code_2t

check_pdf417 :: (Integral a) => [a] -> [F929]
check_pdf417 ds = calcChecksum gp_pdf417 ds

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

main :: IO ()
main = do
  putStrLn $ "RS Code: (" ++ show code_N ++ "," ++ show (code_N - code_2t) ++ ")"
  putStrLn $ "Received message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws)
  let csum = check_pdf417 rws
  putStrLn $ "Checksum : " ++ showF929 csum
  let synd = calcSyndrome f_alpha csum
  putStrLn $ "Syndromes: " ++ showF929 synd
  let sigma_r = errLocator synd
  putStrLn $ "Error locator: " ++ showF929 sigma_r
  let locs = solveErrLocations f_alpha sigma_r
  let locs_r = [code_N-1-k | k <- reverse locs]
  putStrLn $ "Error locations: " ++ show locs_r
  let mtx = errMatrix f_alpha locs synd
  putStrLn "Error matrix:"
  mapM_ (putStrLn . ("   [ " ++) . (++ " ]") . showF929) mtx
  let evs = solveErrMatrix mtx
  let evs_r = reverse evs
  putStrLn $ "Error values: " ++ showF929 evs_r
  let rws_corr = map (fromIntegral . F.toInteger) $ correctErrors (zip locs_r evs_r) $ map (fromInteger . fromIntegral) rws
  putStrLn $ "Corrected message: "
  mapM_ (putStrLn . ("   " ++)) (dumpMsg rws_corr)
  putStrLn $ "Checksum : " ++ showF929 (check_pdf417 rws_corr)

-- EOF
