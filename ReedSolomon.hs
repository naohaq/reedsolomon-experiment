{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

module ReedSolomon
  ( calcChecksum
  , calcSyndrome
  , errLocator
  , solveErrLocations
  , errMatrix
  , solveErrMatrix
  , correctErrors
  , RSdecoder( .. )
  , ReedSolomon( .. )
  , rsDecoder
  ) where

import qualified Polynomial as P
import qualified Language.Haskell.TH as TH
import qualified TypeLevel.Number.Nat as TL

data RSdecoder n k a = RSdecoder { fgen :: Int -> a }

class ReedSolomon a where
  block_N :: a -> Int
  block_K :: a -> Int

instance (TL.Nat n, TL.Nat k) => ReedSolomon (RSdecoder n k a) where
  block_N _ = TL.toInt (undefined :: n)
  block_K _ = TL.toInt (undefined :: k)

gen_poly :: (Num a, TL.Nat n, TL.Nat k) => RSdecoder n k a -> [a]
gen_poly x = foldr P.mul [1] $ map e [1..m]
  where m = block_N x - block_K x
        f = fgen x
        e j = [1, negate (f j)]

calcChecksum :: (Integral b, TL.Nat n, TL.Nat k, Num a, Fractional a) =>
                RSdecoder n k a -> [b] -> [a]
calcChecksum dec ms = mp `P.mod` gp
  where mp = map (fromInteger . fromIntegral) ms
        gp = gen_poly dec

calcSyndrome :: (TL.Nat n, TL.Nat k, Num a) => RSdecoder n k a -> [a] -> [a]
calcSyndrome dec xs = [P.apply xs (f j) | j <- [1..m]]
  where f = fgen dec
        m = block_N dec - block_K dec

solveErrLocations :: (TL.Nat n, TL.Nat k, Num a, Eq a) => RSdecoder n k a -> [a] -> [Int]
solveErrLocations dec csr = [j | j <- [0..(block_N dec-1)], P.apply csr (f j) == 0]
  where f = fgen dec

errLocator :: (TL.Nat n, TL.Nat k, Num a, Fractional a, Eq a) => RSdecoder n k a -> [a] -> [a]
errLocator dec ss = reverse cs
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

errMatrix :: (TL.Nat n, TL.Nat k, Num a) => RSdecoder n k a -> [Int] -> [a] -> [[a]]
errMatrix dec locs ss = [map (fgen dec . (j*)) locs ++ [ss !! (j-1)] | j <- [1..t]]
  where t = length locs

solveErrMatrix :: (Num k, Fractional k, Eq k) => [[k]] -> [k]
solveErrMatrix mtx = map (head . drop n) $ backwardSubst n $ forwardErase n mtx
  where n = length mtx

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (_:xs) = y:xs
replaceAt n y (x:xs) = x : replaceAt (n-1) y xs

addRowTo :: (Num a) => Int -> Int -> a -> [[a]] -> [[a]]
addRowTo x y w rows = replaceAt y r1' rows
  where r0 = rows !! x
        r1 = rows !! y
        r1' = zipWith ((+) . (w*)) r0 r1

multRow :: (Num a) => Int -> a -> [[a]] -> [[a]]
multRow x w rows = replaceAt x r' rows
  where r = rows !! x
        r' = map (w*) r

eraseCols :: (Num a, Fractional a, Eq a) => Int -> Int -> [[a]] -> [[a]]
eraseCols n j mtx = foldr ers mtx' [(j+1)..(n-1)]
  where w = (mtx !! j) !! j
        mtx' = multRow j (1/w) mtx
        ers k m | v /= 0    = addRowTo j k (negate v) m
                | otherwise = m
          where v = (m !! k) !! j

forwardErase :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
forwardErase n mtx = foldr (eraseCols n) mtx (reverse [0..(n-1)])

substCol :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
substCol j mtx = foldr ers mtx [0..(j-1)]
  where ers k m | v /= 0    = addRowTo j k (negate v) m
                | otherwise = m
          where v = (m !! k) !! j

backwardSubst :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
backwardSubst n mtx = foldr substCol mtx [1..(n-1)]

correctErrors :: (Num a) => [(Int,a)] -> [a] -> [a]
correctErrors ers xs = iter 0 ers xs
  where iter _ _  [] = []
        iter _ [] ys = ys
        iter j ((k,e):es) (y:ys) | j == k    = (y - e) : iter (j+1) es ys
                                 | otherwise = y : iter (j+1) ((k,e):es) ys

rsDecoder :: Integer -> Integer -> TH.TypeQ
rsDecoder n k
  | (n <= 0) || (k <= 0) = error "rsDecoder: n and k must be positive numbers."
  | (n <= k)             = error "rsDecoder: n must be greater than k."
  | ((n-k) `mod` 2) /= 0 = error "rsDecoder: n - k must be an even number."
  | otherwise            = [t| RSdecoder $(TL.natT n) $(TL.natT k) |]

-- EOF
