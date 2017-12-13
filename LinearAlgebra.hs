{-# OPTIONS_GHC -Wall #-}

module LinearAlgebra
  ( addRowTo
  , swapRows
  , scaleRow
  , eraseCol
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

findRow :: ([a] -> Bool) -> [[a]] -> Maybe Int
findRow p mtx = iter 0 mtx
  where iter _ [] = Nothing
        iter k (r:rs) | p r       = Just k
                      | otherwise = iter (k+1) rs

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

pivot :: (Num a, Eq a) => Int -> [[a]] -> Maybe [[a]]
pivot n mtx | v == 0     = k >>= (\j -> return $ swapRows n (n+j) mtx)
            | otherwise  = Just mtx
  where v = (mtx !! n) !! n
        k = findRow ((/= 0).(!! n)) (drop n mtx)

eraseCol :: (Num a, Fractional a, Eq a) => Int -> Int -> [[a]] -> Maybe [[a]]
eraseCol n j mtx = do
  pmtx <- pivot j mtx
  let w = (pmtx !! j) !! j
  let smtx = scaleRow j (1/w) pmtx
  return $ foldr ers smtx [(j+1)..(n-1)]
    where ers k m | v /= 0    = addRowTo j k (negate v) m
                  | otherwise = m
            where v = (m !! k) !! j

forwardErase :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> Maybe [[a]]
forwardErase n mtx = L.foldl' f (Just mtx) [0..(n-1)]
  where f v j = v >>= eraseCol n j

substCol :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [[a]]
substCol j mtx = foldr ers mtx [0..(j-1)]
  where ers k m | v /= 0    = addRowTo j k (negate v) m
                | otherwise = m
          where v = (m !! k) !! j

backwardSubst :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> Maybe [[a]]
backwardSubst n mtx = return $ foldr substCol mtx [1..(n-1)]

solve :: (Num a, Fractional a, Eq a) => Int -> [[a]] -> [a] -> Maybe [a]
solve n m v = Just mtx >>= forwardErase n >>= backwardSubst n >>= (return . map last)
  where mtx = zipWith (\rs x -> rs ++ [x]) m v

transpose :: [[a]] -> [[a]]
transpose = L.transpose

-- EOF
