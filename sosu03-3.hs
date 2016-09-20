import Data.Numbers.Primes ( primes )
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.List (intersperse, intercalate)

-- arr = 1 : zipWith (-) (drop 2 primes) arr

-- 元の配列を作る関数。
arr n = (primes !! (n + 1)) - (primes !! n) : zipWith (-) (drop (n + 2) primes) (arr n)

-- ２次元配列にしてみた。
arrarr = map arr [0, 1..]

-- 数値を8進数に変換(ドレミファソラシ＋休符。)
to_senary int = showIntAtBase 8 intToDigit int ""

-- 数字の文字列を音符に変換
to_note str = intercalate "" $ map (\x -> case x of
            '0' -> "c16"
            '1' -> "d16"
            '2' -> "e16"
            '3' -> "f16"
            '4' -> "g16"
            '5' -> "a16"
            '6' -> "b16"
            '7' -> "r16"
            _ -> [x]) str

-- 元の配列を音符に変換
-- Original arr length: 661
getmml n = let intarr = take 661 $ arr n
               str = intercalate "\n" $ map to_senary intarr
               note = to_note str
           in "Track(" ++ (show (n+1)) ++ ") Channel(1)\n" ++ note

-- 出力
main = mapM_ putStrLn $ map getmml [0..13]