module RegCalc where

data Exp = Lit Int 
         | Neg Exp
         | Add Exp Exp 
         | Set Exp
         | Get
    deriving (Eq, Show)

-- 2. Identify/Define the semantic domain
type Domain = Int -> (Int, Int)

-- 3. Define the semantic function
sem :: Exp -> Int -> (Int, Int)
sem (Lit n) m  = (m,n)
sem (Neg e) m  = let (m', n) = sem e m in (m', -n)
sem (Set e) m  = let (_,n)   = sem e m in (n, n)
sem (Add l, r) m = let (ml,nl) = sem l m in
                   let (mr, nr) = sem r ml in (mr, nl+nr)
sem Get m  = (m, m)
