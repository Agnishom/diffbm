{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module StDBM where

import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array (Array, bounds, (!))

import Constraint

data DBM (s :: *) (a :: * -> * -> *)  where
    DBM :: (MArray a Constraint (ST s)) => {
        varCount :: Int,
        canonical :: Bool,
        arr :: a (Int, Int) Constraint
    } -> DBM s a


eqDBM :: (MArray a Constraint (ST s)) => DBM s a -> DBM s a -> ST s Bool
eqDBM dbm1 dbm2 =
    and <$>
        sequence (pure (varCount dbm1 == varCount dbm2) :
        [ (==) <$> readArray (arr dbm1) (i, j) <*> readArray (arr dbm2) (i, j)
        | i <- [0..varCount dbm1], j <- [0..varCount dbm1]
        ])

ltDBM :: (MArray a Constraint (ST s)) => DBM s a -> DBM s a -> ST s Bool
ltDBM dbm1 dbm2 =
    and <$>
        sequence (pure (varCount dbm1 == varCount dbm2) :
        [ (<) <$> readArray (arr dbm1) (i, j) <*> readArray (arr dbm2) (i, j)
        | i <- [0..varCount dbm1], j <- [0..varCount dbm1]
        ])

-- | Given n, return a DBM with n variables and no constraints.
-- | This consists of an (n + 1) x (n + 1) matrix with all (non-diagonal) entries set to Tt.
-- | The diagonal entries are set to (<=, 0).
unConstrained :: (MArray a Constraint (ST s)) => Int -> ST s (DBM s a)
unConstrained n = do
    DBM n True <$> newGenArray ((0, 0), (n, n)) (\(i, j) ->
        if i == j then pure (Constr Le 0) else pure Tt)

-- | Given n, return a DBM with n variables representing the empty set.
emptyZone :: (MArray a Constraint (ST s)) => Int -> ST s (DBM s a)
emptyZone n = do
    DBM n True <$> newGenArray ((0, 0), (n, n)) (\_ -> pure Ff)

-- | Given a DBM n, and a vector of n values, 
-- | check if the vector satisfies the constraints of the DBM.
member :: (MArray a Constraint (ST s)) => DBM s a -> Array Int Double -> ST s Bool
member dbm vec = do
    let n = varCount dbm
    let (_, n') = bounds vec
    if n /= n' then error "member: vector length does not match DBM" else do
        and <$> sequence (
            [ (\x -> satConstr x (vec ! i) (vec ! j)) <$> readArray (arr dbm) (i, j)
            | i <- [1..n], j <- [1..n]
            ] 
         ++ [ (\x -> satConstr  x 0 (vec ! j)) <$> readArray (arr dbm) (0, j) 
            | j <- [1..n] ]
         ++ [ (\x -> satConstr  x (vec ! i) 0) <$> readArray (arr dbm) (i, 0)
            | i <- [1..n] ]
            )