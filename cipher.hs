import Data.Char

-- Enhance the Ceasar cipher to handle upper case letters.
-- The idea I use here is to handle an upper case letter at the
-- highest level (shift) by converting it to the corresponding lower case
-- letter first.

-- The next two functions assume lower case letters, always!
let2int :: Char -> Int
let2int c  = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

{-- Original. Only encodes lower case letters..
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c
--}

-- NOTE! Haskell limitation. Localization is per expression, not per case:
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2let (lowC + delta)
  | otherwise = c
    where
      lowC = (let2int c - delta + n) `mod` 26
      delta = ord 'A' - ord 'a'

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
decode n xs = [shift (-1 * n) x | x <-xs]

my_test_str = "Think like a Fundamentalist Code like a Hacker"
my_test_str_encoded = encode 13 my_test_str

supriz_kayra = encode 8 "Merhaba, Kayra'cigim! Seni cok seven, Baban"
supriz_evren = encode 12 "Merhaba, Evren'cigim! Seni cok seven, Baban"
coz = decode
