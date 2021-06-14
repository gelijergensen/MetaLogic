module LogicSystemSpec where

import qualified Assignment
import Control.Monad (liftM2, replicateM)
import Data.Either (fromRight, isLeft)
import qualified Data.Set as Set
import qualified LogicSystem as LS
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (LS.Frontier a) where
  arbitrary = LS.Frontier . Set.fromList <$> arbitrary

spec :: Spec
spec = do
  describe "LogicSystem.searchWithNeighbors" $ do
    prop "Performs one step of a BFS (visited)" $
      \frs viss ->
        ( LS.visited
            . LS.searchWithNeighbors
              (\i -> LS.Frontier . Set.fromList $ [i - 1, i + 1])
            $ LS.SearchEnv (LS.Frontier frs) viss ::
            Set.Set Int
        )
          == Set.union frs viss
    prop "Performs one step of a BFS (frontier)" $
      \frs viss ->
        ( LS.unFrontier
            . LS.frontier
            . LS.searchWithNeighbors
              (\i -> LS.Frontier . Set.fromList $ [i - 1, i + 1])
            $ LS.SearchEnv (LS.Frontier frs) viss ::
            Set.Set Int
        )
          == Set.difference
            (Set.union (Set.map (subtract 1) frs) (Set.map (+ 1) frs))
            (Set.union frs viss)
