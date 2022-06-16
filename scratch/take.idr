%hide Nat

data Nat: Type where
  Zero: Nat
  Succ: Nat -> Nat

data Fin: Nat -> Type where
  FZero: Fin (Succ n)
  FSucc: Fin n -> Fin (Succ n)

data Vec: Nat -> Type -> Type where
  Nil: Vec Zero a
  VCons: a -> Vec n a -> Vec (Succ n) a

toNat: Fin n -> Nat
toNat FZero = Zero
toNat (FSucc n) = Succ (toNat n)

take: { n: Nat } -> (m: Fin (Succ n)) -> Vec n a -> Vec (toNat m) a
take FZero _ = []
take (FSucc _) [] impossible
take (FSucc n) (VCons x xs) = VCons x (take n xs)
