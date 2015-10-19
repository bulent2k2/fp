-- reasoning about programs.. (hw/chapter 12)

data Nat = Zero | Succ Nat deriving(Show)

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

-- Show property: add n (Succ m) = Succ (add n m)
{- Base case: 

  add Zero (Succ m)     =? Succ (add Zero m)
  <Apply add>            = Succ m
  <Unapply add>          = Succ (add Zero m)

 - Inductive case:

  add (Succ n) (Succ m) =? Succ (add (Succ n) m)
  <Apply add>            = Succ (add n (Succ m))
  <Induction hypothesis> = Succ (Succ (add n m))
  <Unapply add>          = Succ (add (Succ n) m)
-}

-- Show commutativity: add n m = add m n
{- Base case: 

  add Zero m  =? add m Zero
  <Apply add> = m
  <prop?>     = add m Zero

 - Inductive case:

  add (Succ n) m  =? add m (Succ n)
  <Apply add>      = Succ (add n m)
  <Inductive hypo> = Succ (add m n)
  <prop>           = add (m (Succ n))
-}
