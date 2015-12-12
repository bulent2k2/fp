
data Person = P { name :: String
                , addr :: Address
                , salary :: Int
                , hoby :: [Hoby] }
data Address = A { road :: String
                 , city :: String
                 , zip  :: String }
data Hoby = H { hobyName :: String }

-- addr :: Person -> Address
-- ??

setName :: String -> Person -> Person
setName n p = p { name = n }

-- setZip :: String -> Person -> Person
