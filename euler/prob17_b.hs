import Data.Char (digitToInt)

units =     [0, 3, 3, 5, 4, 4, 3, 5, 5, 4]
teens =     [3, 6, 6, 8, 8, 7, 7, 9, 8, 8]
tens =      [6, 6, 5, 5, 5, 7, 6, 6]

digits :: String -> [Int]
digits (x:xs) = digitToInt x : digits xs
digits [] = []

process :: [Int] -> Int
process arr = processHelper arr (length arr) where
    processHelper :: [Int] -> Int -> Int
    processHelper (x:xs) arrLength = case arrLength of
        3 -> (processHelper [x] 1) + 7 {-hundred-} + let restLen = processHelper xs 2 in case restLen of
            0 -> 0
            _ -> restLen + 3 --include and
        2 -> case x of
            0 -> processHelper xs 1
            1 -> teens!!(head xs)
            _ -> tens!!(x-2) + processHelper xs 1
        1 -> units!!x
        _ -> undefined

main :: IO ()
main = print $(sum$(map) (process.digits.show) [1..999]) + 11 --onethousand
