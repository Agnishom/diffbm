module DBM where

import Data.Array
import Data.List (nub)
import Control.Monad
import Data.Array.MArray
import Data.Function ((&))

import Constraint
import Data.Array.ST (runSTArray)

data DBM = DBM {
    varCount :: Int,
    canonical :: Bool,
    arr :: Array (Int, Int) Constraint
} deriving (Eq, Show)

instance Ord DBM where
    (<) :: DBM -> DBM -> Bool
    dbm1 < dbm2 = and [ arr dbm1 ! (i, j) < arr dbm2 ! (i, j) | i <- [0..varCount dbm1], j <- [0..varCount dbm1] ]

    (<=) :: DBM -> DBM -> Bool
    dbm1 <= dbm2 = dbm1 < dbm2 || dbm1 == dbm2

-- | Given n, return a DBM with n variables and no constraints.
-- | This consists of an (n + 1) x (n + 1) matrix with all (non-diagonal) entries set to Tt.
-- | The diagonal entries are set to (<=, 0).
unconstrained :: Int -> DBM
unconstrained n = DBM n True $ array ((0, 0), (n, n)) [((i, j), if i == j then Constr Le 0 else Tt) | i <- [0..n], j <- [0..n]]

-- | Given n, return a DBM with n variables representing the empty set.
emptyZone :: Int -> DBM
emptyZone n = DBM n True $ array ((0, 0), (n, n)) [((i, j), Ff) | i <- [0..n], j <- [0..n]]

-- | Given a DBM n, and a vector of n values, 
-- | check if the vector satisfies the constraints of the DBM.
member :: DBM -> Array Int Double -> Bool
member dbm vec
    | bounds vec /= (1, varCount dbm) = error "member: vector length does not match DBM variable count"
    | otherwise =
        and [ satConstr (arr dbm ! (i, j)) (vec ! i) (vec ! j) | i <- [1..varCount dbm], j <- [1..varCount dbm] ]
        && and [ satConstr (arr dbm ! (0, j)) 0 (vec ! j) | j <- [1..varCount dbm] ]
        && and [ satConstr (arr dbm ! (i, 0)) (vec ! i) 0 | i <- [1..varCount dbm] ]

-- | Given (i, j) and a constraint, check if the constraint is satisfied by the DBM.
-- | This is equivalent to asking if (isDBMEmpty $ constrain i j constr dbm)
satisfiesConstraint :: Int -> Int -> Constraint -> DBM -> Bool
satisfiesConstraint i j constr dbm 
    | canonical dbm = isNegative $ (arr dbm ! (j, i)) `addConstr` constr -- the reversal of the indices is intentional
    | otherwise = satisfiesConstraint i j constr $ canonicalize dbm

-- | is DBM1 a subset of DBM2?
subsetDBM :: DBM -> DBM -> Bool
subsetDBM dbm1 dbm2
    | varCount dbm1 /= varCount dbm2 = error "subsetDBM: DBMs have different variable counts"
    | canonical dbm1 && canonical dbm2 = dbm1 <= dbm2
    | otherwise = subsetDBM (canonicalize dbm1) (canonicalize dbm2)

-- | is DBM empty?
isDBMEmpty :: DBM -> Bool
isDBMEmpty dbm
    | canonical dbm = isNegative $ arr dbm ! (0, 0)
    | otherwise = isDBMEmpty $ canonicalize dbm

-- | is DBM universal?
isDBMUniversal :: DBM -> Bool
isDBMUniversal dbm
    | canonical dbm = arr dbm == arr (unconstrained $ varCount dbm)
    | otherwise = isDBMUniversal $ canonicalize dbm

-- | Add a constraint to a DBM.
constrain :: Int -> Int -> Constraint -> DBM -> DBM
constrain i j constr dbm
    | arr dbm ! (i, j) <= constr = dbm -- the existing constraint is tighter, so no need to update
    | otherwise = dbm { arr = arr dbm // [((i, j), constr)], canonical = False }

{-

How does the following work in O(n^2)?

(1) Note that the following code is oblivious to the order in which the vertices are arranged:

```
forM_ [0..n] $ \k ->
        forM_ [0..n] $ \i ->
            forM_ [0..n] $ \j -> do
                m_ij <- readArray mat (i, j)
                m_jk <- readArray mat (j, k)
                m_ik <- readArray mat (i, k)
                writeArray mat (i, j) $ m_ij `intersectConstr` addConstr m_ik m_jk
```

(2) So, WLOG, assume that the last two vertices that are touched are i and j.
(3) For the first (n - 2) iterations of the outer loop, the last line does nothing
(4) Therefore, it is good enough to just run the last two iterations of the outer loop

-}

-- | Adds a constraint to the DBM, and then canonicalizes it.
-- | The canonicalization works in O(n^2)
constrainCanon :: Int -> Int -> Constraint -> DBM -> DBM
constrainCanon i j constr dbm =
    if arr dbm ! (i, j) <= constr then dbm
    else 
    let mat = arr dbm // [((i, j), constr)]
            & closeMany [i, j]
    in dbm { arr = mat, canonical = True }

constrainCannonMany :: [((Int, Int), Constraint)] -> DBM -> DBM
constrainCannonMany newConstrs dbm =
    let newConstrs' = filter (\((i, j), constr) -> constr < arr dbm ! (i, j)) newConstrs
        mat = arr dbm // newConstrs'
            & closeMany (nub . join $ [[i, j] | ((i, j), _) <- newConstrs'])
    in dbm { arr = mat, canonical = True }

-- | Let time elapse, i.e, for each i, let x_i < 0 + infinity
-- | This is done by setting the (i, 0) entries to Tt.
elapse :: DBM -> DBM
elapse dbm = dbm { arr = arr dbm // [((i, 0), Tt) | i <- [1..varCount dbm]] }

-- | All possible past values
-- | Given a DBM dbm, u \in (past dbm) iff there exists some delay such that u + delay \in dbm
-- | Sets all constrains about x_i to 0 <= x_i + 0
past :: DBM -> DBM
past dbm = dbm { arr = arr dbm // [((0, i), Constr Le 0) | i <- [1..varCount dbm]], canonical = False }

pastCannon :: DBM -> DBM
pastCannon dbm = 
    let mat = arr dbm // [((0, i), Constr Le 0) | i <- [1..varCount dbm]]
            & closePast i
    in dbm { arr = mat, canonical = True }

-- | (resetD i d) changes the dbm so that the x_i = d.
-- | To do this, set arr[i, j] := (Le, d) `addConstr` arr[0, j]
-- |             and arr[j, i] := (Le, -d) `addConstr` arr[j, 0]
resetD :: Double -> DBM -> Int -> DBM
resetD d dbm i = dbm {
    arr = arr dbm //
        ([((i, j), Constr Le d `addConstr` (arr dbm ! (0, j))) | j <- [0..varCount dbm]]
        ++ [((j, i), Constr Le (-d) `addConstr` (arr dbm ! (j, 0))) | j <- [0..varCount dbm]])
    }

-- | reset the given clock to 0
reset :: DBM -> Int -> DBM
reset = resetD 0

-- | Canonicalize a DBM. This runs the Floyd-Warshall algorithm on the DBM.
canonicalize :: DBM -> DBM
canonicalize (DBM n _ mat) = DBM n True $ floydWarshall mat

floydWarshall :: Array (Int, Int) Constraint -> Array (Int, Int) Constraint
floydWarshall mat' = runSTArray $ do
    mat <- thaw mat'
    forM_ [0..n] $ \k ->
        forM_ [0..n] $ \i ->
            forM_ [0..n] $ \j -> do
                m_ij <- readArray mat (i, j)
                m_jk <- readArray mat (j, k)
                m_ik <- readArray mat (i, k)
                writeArray mat (i, j) $ m_ij `intersectConstr` addConstr m_ik m_jk
    pure mat
    where ((_, _), (_, n)) = bounds mat'

-- | This is equivalent to running the two inner loops of Floyd-Warshall, 
-- | for a fixed value of k
close1 :: Int -> Array (Int, Int) Constraint -> Array (Int, Int) Constraint
close1 k mat' = runSTArray $ do
    mat <- thaw mat'
    forM_ [0..n] $ \i ->
        forM_ [0..n] $ \j -> do
            m_ij <- readArray mat (i, j)
            m_ik <- readArray mat (i, k)
            m_kj <- readArray mat (k, j)
            writeArray mat (i, j) $ m_ij `intersectConstr` addConstr m_ik m_kj
    pure mat
    where ((_, _), (_, n)) = bounds mat'

closeMany :: [Int] -> Array (Int, Int) Constraint -> Array (Int, Int) Constraint
closeMany ks mat' = runSTArray $ do
    mat <- thaw mat'
    forM_ ks $ \k ->
        forM_ [0..n] $ \i ->
            forM_ [0..n] $ \j -> do
                m_ij <- readArray mat (i, j)
                m_ik <- readArray mat (i, k)
                m_kj <- readArray mat (k, j)
                writeArray mat (i, j) $ m_ij `intersectConstr` addConstr m_ik m_kj
    pure mat
    where ((_, _), (_, n)) = bounds mat'

closePast :: Int -> Array (Int, Int) Constraint -> Array (Int, Int) Constraint
closePast i mat' = runSTArray $ do
    mat <- thaw mat'
    forM_ [1..n] $ \j -> do
        m_ji <- readArray mat (j, i)
        m_0i <- readArray mat (0, i)
        writeArray mat (0, i) $ m_0i `intersectConstr` m_ji
    pure mat
    where ((_, _), (_, n)) = bounds mat'