{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module LogicSystem where

import qualified Data.Set as Set

class LogicSystem t where
  data Operator t :: * -> *
  data Formula t :: * -> *
  data Rule t :: * -> *

  identifier :: Formula t a -> a

  -- subformulas :: Formula t a -> [Formula t a]
  rewriteRules :: t -> [Rule t a]
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

rewrite ::
  (Ord (Formula t a), LogicSystem t) =>
  t ->
  Formula t a ->
  Set.Set (Formula t a)
rewrite sys = extractSearch . searchWithNeighbors (rewriteAll sys) . startSearch
  where
    extractSearch env = Set.union (visited env) (unFrontier $ frontier env)
    rewriteAll sys x =
      Frontier . Set.fromList . map (`runRule` x) . rewriteRules $ sys
    startSearch x = SearchEnv (Frontier $ Set.singleton x) Set.empty

searchWithNeighbors :: Ord a => (a -> Frontier a) -> SearchEnv a -> SearchEnv a
searchWithNeighbors f env =
  SearchEnv
    { frontier = Frontier $ Set.difference (unFrontier newFrontier) newVisited,
      visited = newVisited
    }
  where
    newFrontier = foldMap f (frontier env)
    newVisited = Set.union (visited env) (unFrontier $ frontier env)
