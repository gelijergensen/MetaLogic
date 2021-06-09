module Assignment where

import qualified Data.Map as Map

newtype Assignment a = Assignment (Map.Map a Bool) deriving (Eq, Show)

type AssignError = String

emptyAssignment :: Assignment a
emptyAssignment = Assignment Map.empty

assign :: (Ord a, Show a) => Assignment a -> a -> Either AssignError Bool
assign assignment@(Assignment as) a = case Map.lookup a as of
  Just x -> Right x
  Nothing -> Left $ assignError assignment a

assignError :: Show a => Assignment a -> a -> AssignError
assignError _ a = "AssignError: Value of " ++ show a ++ " not given"
