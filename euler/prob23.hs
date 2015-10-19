import qualified Data.MemoCombinators as Memo
import Data.List (nub)

abundant:: Int -> Bool
abundant = Memo.integral (\x -> (sum . factors) x > x)

factors:: Int -> [Int]
factors n = concat [nub [x,q] | x <- [2.. floor . sqrt $ fromIntegral n], let (q,r) = quotRem n x, r == 0]

summable:: Int -> Bool
summable n = or . map (\x -> abundant x && abundant (n-x)) $ [1.. div n 2]

solve:: Int
solve = sum . filter (not . summable) $ [1..20161]

-- *Main Data.List> solve
-- 4179871
-- (21.17 secs, 31236128992 bytes)
