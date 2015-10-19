import Prelude hiding (return)

type Parser a = String -> [(a,String)]

item :: Parser Char
item = \ input -> case input of 
  []     -> []
  (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \ inp -> []

return :: a -> Parser a
return v = \ inp -> [(v, inp)]

-- p if it succeeds, q otherwise:
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp -> case p inp of
  []        -> parse q inp
  [(v,out)] -> [(v,out)]

parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp

-- sequencing
{-
(>>=) :: Parser a -> (a -> Parser b) -> Parser b

p >>= f = \ inp -> case parse p inp of
  []         -> []
  [(v, out)] -> parse (f v) out
-}

{-
parser1 :: Parser (Char,Char)
parser1 = do
  x <- item
  item
  y <- item
  return (x,y)

test1 = parse parser1 "abcdef"
-}
