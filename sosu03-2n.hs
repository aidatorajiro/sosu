import Data.Numbers.Primes ( primes )
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (intersperse, intercalate)

-- arr = 1 : zipWith (-) (drop 2 primes) arr

-- 元の配列を作る関数。
arr n = (primes !! (n + 1)) - (primes !! n) : zipWith (-) (drop (n + 2) primes) (arr n)

-- ２次元配列にしてみた。
arrarr = map arr [0, 1..]

-- 出力
main = mapM_ print $ take 100 $ arr 0