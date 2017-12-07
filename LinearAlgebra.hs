{-# OPTIONS_GHC -Wall #-}

module LinearAlgebra
  ( addRowTo
  , swapRows
  , scaleRow
  , eraseCols
  , forwardErase
  , substCol
  , backwardSubst
  , solve
  , transpose
  ) where

import qualified Data.List as L

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 y (_:xs) = y:xs
replaceAt n y (x:xs) = x : replaceAt (n-1) y xs

addRowTo :: (Num a) => Int -> Int -> a -> [[a]] -> [[a]]
addRowTo x y w rows = replaceAt y r1' rows
  where r0 = rows !! x
        r1 = rows !! y
        r1' = zipWith ((+) . (w*)) r0 r1

swapRows :: Int -> Int -> [[a]] -> [[a]]
swapRows j k rows = replaceAt k r0 $ replaceAt j r1 rows
  where r0 = rows !! j
        r1 = rows !! k

scaleRow :: (Num a) => Int -> a -> [[a]] -> [[a]]
scaleRow x w rows = replaceAt x r' rows
  where r = rows !! x
        r' = map (w*) r

eraseCols :: (Num a, Fractional a, Eq a) => Int -> Int -> [[a]] -> [[a]]
eraseCols n j mtx = foldr ers mtx' [(j+1)..(n-1)]
  where w = (mtx !! j) !! j
        mtx' = scaleRow j (1/w) mtx
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

solve :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [a] -> [a]
solve n m v = map last $ backwardSubst n $ forwardErase n mtx
  where mtx = zipWith (\rs x -> rs ++ [x]) m v

transpose :: [[a]] -> [[a]]
transpose = L.transpose

-- EOF
