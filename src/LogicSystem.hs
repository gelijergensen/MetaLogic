{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module LogicSystem where

import qualified Data.Set as Set
import qualified RApplicative as RApp

class LogicSystem t where
  data Formula t :: * -> *
  data Rule t :: * -> *

  mapFormula ::
    Ord (Formula t a) =>
    (Formula t a -> Frontier (Formula t a)) ->
    Formula t a ->
    Frontier (Formula t a)
  rewriteRules :: Eq a => t -> [Rule t a]
  runRule :: Rule t a -> Formula t a -> Formula t a

data SearchEnv a = SearchEnv
  { frontier :: Frontier a,
    visited :: Set.Set a
  }
  deriving (Eq, Show)

newtype Frontier a = Frontier
  { unFrontier :: Set.Set a
  }
  deriving (Eq, Show)

instance Ord a => Semigroup (Frontier a) where
  Frontier xs <> Frontier ys = Frontier $ Set.union xs ys

instance Ord a => Monoid (Frontier a) where
  mempty = Frontier Set.empty

instance Foldable Frontier where
  foldMap f (Frontier xs) = foldMap f xs

instance RApp.RApplicative Frontier where
  type RApplicativeCtxt Frontier a = Ord a
  pure = Frontier . Set.singleton
  fmap f (Frontier x) = Frontier $ Set.map f x
  liftA2 f (Frontier xs) (Frontier ys) =
    Frontier $ Set.map (uncurry f) $ Set.cartesianProduct xs ys

data Completeness a = Complete a | Incomplete a deriving (Eq, Show)

instance Functor Completeness where
  fmap f (Complete a) = Complete $ f a
  fmap f (Incomplete a) = Incomplete $ f a

rewriteOnceAtRoot ::
  (Eq a, Ord (Formula t a), LogicSystem t) =>
  t ->
  Formula t a ->
  Set.Set (Formula t a)
rewriteOnceAtRoot sys =
  extractSearch . searchWithNeighbors (applyAllRules sys) . startSearch

rewriteOnce ::
  (Eq a, Ord (Formula t a), LogicSystem t) =>
  t ->
  Formula t a ->
  Set.Set (Formula t a)
rewriteOnce sys =
  extractSearch
    . searchWithNeighbors (mapFormula (applyAllRules sys))
    . startSearch

{- Caution: Assumes that the rewriting process terminates!
   Use rewriteNTimes if process might not terminate -}
rewrite ::
  (Eq a, Ord (Formula t a), LogicSystem t) =>
  t ->
  Formula t a ->
  Set.Set (Formula t a)
rewrite sys =
  extractSearch . totalSearch (mapFormula (applyAllRules sys)) . startSearch

rewriteNTimes ::
  (Eq a, Ord (Formula t a), LogicSystem t) =>
  t ->
  Int ->
  Formula t a ->
  Completeness (Set.Set (Formula t a))
rewriteNTimes sys n =
  fmap extractSearch
    . searchNTimes n (mapFormula (applyAllRules sys))
    . startSearch

applyAllRules ::
  (Eq a, Ord (Formula t a), LogicSystem t) =>
  t ->
  Formula t a ->
  Frontier (Formula t a)
applyAllRules sys x =
  Frontier . Set.fromList . map (`runRule` x) . rewriteRules $ sys

-- generic BFS searching
extractSearch :: Ord a => SearchEnv a -> Set.Set a
extractSearch env = Set.union (visited env) (unFrontier $ frontier env)

startSearch :: a -> SearchEnv a
startSearch x = SearchEnv (Frontier $ Set.singleton x) Set.empty

-- Note: assumes that the search process terminates
totalSearch :: Ord a => (a -> Frontier a) -> SearchEnv a -> SearchEnv a
totalSearch f env = go f env
  where
    go _ env
      | null (frontier env) = env
    go f env = go f $ searchWithNeighbors f env

searchNTimes ::
  Ord a => Int -> (a -> Frontier a) -> SearchEnv a -> Completeness (SearchEnv a)
searchNTimes n f env = checkCompleteness $ go n f env
  where
    checkCompleteness env
      | null (frontier env) = Complete env
      | otherwise = Incomplete env
    go 0 _ env = env
    go _ _ env
      | null (frontier env) = env
    go n f env = go (n - 1) f $ searchWithNeighbors f env

searchWithNeighbors :: Ord a => (a -> Frontier a) -> SearchEnv a -> SearchEnv a
searchWithNeighbors f env =
  SearchEnv
    { frontier = Frontier $ Set.difference (unFrontier newFrontier) newVisited,
      visited = newVisited
    }
  where
    newFrontier = foldMap f (frontier env)
    newVisited = Set.union (visited env) (unFrontier $ frontier env)
