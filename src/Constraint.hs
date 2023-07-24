module Constraint where

data Comp = Lt | Le
    deriving (Eq, Ord, Show)

data Constraint = 
      Ff -- (<, -∞)
    | Tt -- (<, ∞)
    | Constr Comp Double -- (</<=, m)
    deriving (Eq, Show)

instance Ord Constraint where
    (<) :: Constraint -> Constraint -> Bool
    _ < Ff = False
    Ff < _ = True
    Tt < _ = False
    _ < Tt = True
    Constr comp1 m1 < Constr comp2 m2
        | m1 < m2 = True
        | m1 > m2 = False
        | otherwise = comp1 < comp2
    (<=) :: Constraint -> Constraint -> Bool
    c1 <= c2 = c1 < c2 || c1 == c2

isNegative :: Constraint -> Bool
isNegative Ff = True
isNegative (Constr Le m) = m <= 0
isNegative (Constr Lt m) = m < 0
isNegative _ = False

intersectConstr :: Constraint -> Constraint -> Constraint
intersectConstr = min

addConstr :: Constraint -> Constraint -> Constraint
addConstr (Constr comp1 m1) (Constr comp2 m2) = Constr (min comp1 comp2) (m1 + m2)
addConstr Tt _ = Tt
addConstr _ Tt = Tt
addConstr Ff _ = Ff
addConstr _ Ff = Ff

satConstr :: Constraint -> Double -> Double -> Bool
satConstr Ff _ _ = False
satConstr Tt _ _ = True
satConstr (Constr comp m) x y
    | comp == Lt = x < y + m
    | comp == Le = x <= y + m