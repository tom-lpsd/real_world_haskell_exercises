module BloomFilter.Easy 
    (
      suggestSizing
    , sizings
    , easyList
    , B.Bloom
    , B.length
    , B.elem
    , B.notElem
    ) where

import BloomFilter.Hash (Hashable, doubleHash)
import Data.List (genericLength)
import Data.Maybe (catMaybes)
import Data.Word (Word32)
import qualified BloomFilter as B

easyList :: (Hashable a) =>
            Double
         -> [a]
         -> Either String (B.Bloom a)
easyList errRate values =
    case suggestSizing (genericLength values) errRate of
      Left err -> Left err
      Right (bits, hashes) -> Right filt
          where filt = B.fromList (doubleHash hashes) bits values

suggestSizing
    :: Integer
    -> Double
    -> Either String (Word32, Int)
suggestSizing capacity errRate
    | capacity <= 0 = Left "capacity too small"
    | errRate <= 0 || errRate >= 1 = Left "invalid error rate"
    | null saneSizes = Left "capacity too large"
    | otherwise = Right (minumum saneSizes)
    where saneSizes = catMaybes . map sanitize $ sizings capacity errRate
          sanitize (bits, hashes)
              | bits > maxWord32 - 1 = Nothing
              | otherwise = Just (cailing bits, truncate hashes)
              where maxWord32 = fromIntegral (maxBouns :: Word32)

sizings :: Integer -> Double -> [(Double, Double)]
sizings capacity errRate =
    [(((-k) * cap / log (1 - (errRate ** (1/k)))), k) | k <- [1..50]]
    where cap = fromIntegral capacity