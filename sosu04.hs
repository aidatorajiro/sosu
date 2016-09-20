import Data.Numbers.Primes ( primes )
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (intersperse, intercalate)

import Data.List

bits :: Integer -> [Integer]
bits e = reverse $ bits' e
    where
    bits' 0 = []
    bits' x = (x `mod` 2) : bits' (x `div` 2)

modexp_binary :: Integer -> Integer -> Integer -> Integer
modexp_binary x e m = foldl' ope 1 $ bits e
    where
    ope n 0 = n^2 `mod` m
    ope n 1 = n^2 `mod` m * x `mod` m

modexp_normal x e m = x^e `mod` m

arr n = modexp_binary (primes !! n) (primes !! (n + 1)) (primes !! (n + 2)) : zipWith3 (\ x y z -> modexp_binary x y z) (arr n) (drop (n + 3) primes) (drop (n + 4) primes)

main = print $ take 1000 $ arr 0