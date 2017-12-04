
module MyUtil where

import qualified Data.Word as W

fromW8toHex :: W.Word8 -> String
fromW8toHex x = [letters !! (fromIntegral hi), letters !! (fromIntegral lo)]
  where letters = "0123456789abcdef"
        (hi,lo) = x `divMod` 16

rjustify :: Int -> String -> String
rjustify m str = replicate (m-n) ' ' ++ str
  where n = length str

joinStr :: String -> [String] -> String
joinStr _     []    = ""
joinStr delim words = foldr1 (\a b->a ++ delim ++ b) words

-- EOF
