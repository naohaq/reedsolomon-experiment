{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module ReedSolomon
  ( calcChecksum
  , calcSyndrome
  , calcErrors
  , correctErrors
  , encodeBlock
  , RScode ( .. )
  , ReedSolomon ( .. )
  , rsCode
  ) where

import qualified Polynomial as P
import qualified LinearAlgebra as LA
import qualified Language.Haskell.TH as TH
import qualified TypeLevel.Number.Nat as TL

data RScode n k a = RScode { fgen :: Int -> a }

class ReedSolomon c a where
  block_N :: c a -> Int
  block_K :: c a -> Int
  generator :: c a -> (Int -> a)

instance (TL.Nat n, TL.Nat k, Num a, Fractional a, Eq a) =>
         ReedSolomon (RScode n k) a where
  block_N _ = TL.toInt (undefined :: n)
  block_K _ = TL.toInt (undefined :: k)
  generator c = fgen c

gen_poly :: (Num a, ReedSolomon c a) => c a -> [a]
gen_poly rsc = foldr P.mul [1] $ map e [1..m]
  where m = block_N rsc - block_K rsc
        f = generator rsc
        e j = [1, negate (f j)]

encodeBlock :: (Num a, Fractional a, ReedSolomon c a) => c a -> [a] -> [a]
encodeBlock rsc mp = mp' ++ (map negate res)
  where k   = block_K rsc
        m   = block_N rsc - k
        gp  = gen_poly rsc
        mp' = take k mp
        res = (mp' ++ replicate m 0) `P.mod` gp

calcChecksum :: (Num a, Fractional a, ReedSolomon c a) => c a -> [a] -> [a]
calcChecksum dec mp = mp `P.mod` gp
  where gp = gen_poly dec

calcSyndrome :: (Num a, ReedSolomon c a) => c a -> [a] -> [a]
calcSyndrome dec xs = [P.apply xs (f j) | j <- [1..m]]
  where f = generator dec
        m = block_N dec - block_K dec

solveErrLocations :: (Num a, Eq a, ReedSolomon c a) => c a -> [a] -> [Int]
solveErrLocations dec csr = [j | j <- [0..(block_N dec-1)], P.apply csr (f j) == 0]
  where f = generator dec

calcErrLocator :: (Num a, Fractional a, Eq a, ReedSolomon c a) => c a -> [a] -> [a]
calcErrLocator dec ss = reverse cs
  where m = block_N dec - block_K dec
        (_, cs, _, _) = foldr (errLocator_sub ss) (1,[1],[1],1) [(m-1),(m-2)..0]

errLocator_sub :: (Num k, Fractional k, Eq k) => [k] -> Int -> (Int,[k],[k],k) -> (Int,[k],[k],k)
errLocator_sub ss n (m,cs,bs,b) | d == 0    = (m+1,cs ,bs,b)
                                | otherwise = (1  ,cs',cs,d)
  where k = length cs - 1
        d = sum $ zipWith (*) cs (drop (n-k) ss)
        e = d/b
        bs' = map (e*) bs `P.mul` (1 : replicate m 0)
        cs' = cs `P.sub` bs'

calcErrMatrix :: (ReedSolomon c a) => c a -> [Int] -> [[a]]
calcErrMatrix dec locs = [map (f . (j*)) locs | j <- [1..t]]
  where f = generator dec
        t = length locs

solveErrMatrix :: (Num k, Fractional k, Eq k) => [[k]] -> [k] -> [k]
solveErrMatrix mtx ss = LA.solve n mtx (take n ss)
  where n = length mtx

calcErrors :: (Num a, Fractional a, Eq a, ReedSolomon c a) => c a -> [a] -> [(Int,a)]
calcErrors dec synd = zip locs_r evs_r
  where sig_r  = calcErrLocator dec synd
        locs   = solveErrLocations dec sig_r
        locs_r = [block_N dec - 1 - k | k <- reverse locs]
        mtx    = calcErrMatrix dec locs
        evs_r  = reverse $ solveErrMatrix mtx synd

correctErrors :: (Num a) => [(Int,a)] -> [a] -> [a]
correctErrors ers xs = iter 0 ers xs
  where iter _ _  [] = []
        iter _ [] ys = ys
        iter j ((k,e):es) (y:ys) | j == k    = (y - e) : iter (j+1) es ys
                                 | otherwise = y : iter (j+1) ((k,e):es) ys

rsCode :: Integer -> Integer -> TH.TypeQ
rsCode n k
  | (n <= 0) || (k <= 0) = error "rsCode: n and k must be positive numbers."
  | (n <= k)             = error "rsCode: n must be greater than k."
  | ((n-k) `mod` 2) /= 0 = error "rsCode: n - k must be an even number."
  | otherwise            = [t| RScode $(TL.natT n) $(TL.natT k) |]

-- EOF
