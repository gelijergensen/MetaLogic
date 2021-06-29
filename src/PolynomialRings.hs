{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module PolynomialRings where

import qualified Data.Bifunctor as Bifunc
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import qualified ErrorHandling as EH
import qualified Interpreter
import qualified LogicSystem as LS
import qualified RApplicative as RApp

data PolynomialRings = PolynomialRings deriving (Eq, Show)

type Polynomial = LS.Formula PolynomialRings

instance LS.LogicSystem PolynomialRings where
  data Formula PolynomialRings a
    = ZERO
    | ONE
    | CONSTANT a
    | VARIABLE a
    | NEG (Polynomial a)
    | PLUS (MultiSet.MultiSet (Polynomial a))
    | TIMES (MultiSet.MultiSet (Polynomial a))
    deriving (Eq, Ord)

  newtype Rule PolynomialRings a = Rule
    { runRule ::
        Polynomial a ->
        Polynomial a
    }
  type RuleConstraint PolynomialRings a = Ord a

  mapFormula = mapFormula
  rewriteRules =
    const . map Rule $
      [ _negNeg,
        _emptyPlus,
        _plusZero,
        _plusNeg,
        _plusSingle,
        _combinePlus,
        _emptyTimes,
        _timesOne,
        _timesZero,
        _timesNeg,
        _timesSingle,
        _combineTimes,
        _distributive
      ]
  runRule = runRule

instance Show a => Show (Polynomial a) where
  showsPrec d ZERO = showString "0"
  showsPrec d ONE = showString "1"
  showsPrec d (CONSTANT a) = showsPrec d a
  showsPrec d (VARIABLE a) = showsPrec d a
  showsPrec d (NEG a) =
    showParen (d > app_prec) $
      showString "-" . showsPrec d a
    where
      app_prec = 10
  showsPrec d (PLUS as) =
    showParen (d > app_prec) $ showOccurList $ MultiSet.toOccurList as
    where
      showOccurList [] s = s
      showOccurList (x : xs) s =
        showTerm x $ _showList xs
        where
          _showList [] = s
          _showList (y : ys) = case y of
            (NEG y', i) -> showString " - " . showTerm (y', i) $ _showList ys
            _ -> showString " + " . showTerm y $ _showList ys
      showTerm (x, 1) = showsPrec (app_prec + 1) x
      showTerm (x, i) = case x of
        NEG x' ->
          showString "-" . shows i . showString "*" . showsPrec (app_prec + 1) x'
        _ -> shows i . showString "*" . showsPrec (app_prec + 1) x
      app_prec = 9
  showsPrec d (TIMES as) =
    showParen (d > app_prec) $ showOccurList $ MultiSet.toOccurList as
    where
      showOccurList [] s = s
      showOccurList (x : xs) s =
        showTerm x $ _showList xs
        where
          _showList [] = s
          _showList (y : ys) = showString "*" . showTerm y $ _showList ys
      showTerm (x, 1) = showsPrec (app_prec + 1) x
      showTerm (x, i) = showsPrec (app_prec + 1) x . showString "^" . shows i
      app_prec = 10

mapFormula ::
  Ord (Polynomial a) =>
  (Polynomial a -> LS.Frontier (Polynomial a)) ->
  Polynomial a ->
  LS.Frontier (Polynomial a)
mapFormula f term@(TIMES xs) =
  f term <> RApp.fmap TIMES (mapFormulaMultiSet f xs)
mapFormula f term@(PLUS xs) =
  f term <> RApp.fmap PLUS (mapFormulaMultiSet f xs)
mapFormula f term@(NEG x) = f term <> RApp.fmap NEG (mapFormula f x)
mapFormula f term = f term

-- Super ugly mashing around of types
-- Sadly, RApp.sequenceA would require a constrained version of Traversable, too
mapFormulaMultiSet ::
  Ord (Polynomial a) =>
  (Polynomial a -> LS.Frontier (Polynomial a)) ->
  MultiSet.MultiSet (Polynomial a) ->
  LS.Frontier (MultiSet.MultiSet (Polynomial a))
mapFormulaMultiSet f =
  LS.Frontier
    . Set.fromList
    . multiSetCartesianProductWith (Set.toList . LS.unFrontier . mapFormula f)

multiSetCartesianProductWith ::
  Ord a => (a -> [a]) -> MultiSet.MultiSet a -> [MultiSet.MultiSet a]
multiSetCartesianProductWith f =
  map
    MultiSet.fromOccurList
    . LS.cartesianProduct
    . map ((\(xs, c) -> map (,c) xs) . Bifunc.first f)
    . MultiSet.toOccurList

isNeg :: Polynomial a -> Bool
isNeg (NEG _) = True
isNeg _ = False

isPlus :: Polynomial a -> Bool
isPlus (PLUS _) = True
isPlus _ = False

isTimes :: Polynomial a -> Bool
isTimes (TIMES _) = True
isTimes _ = False

---------- Rewrite Rules ----------

_negNeg :: Polynomial a -> Polynomial a
_negNeg (NEG (NEG x)) = x
_negNeg x = x

_emptyPlus :: Polynomial a -> Polynomial a
_emptyPlus x@(PLUS xs)
  | MultiSet.null xs = ZERO
  | otherwise = x
_emptyPlus x = x

_plusZero :: Eq a => Polynomial a -> Polynomial a
_plusZero (PLUS xs) = PLUS $ MultiSet.filter (/= ZERO) xs
_plusZero x = x

_plusNeg :: Ord a => Polynomial a -> Polynomial a
_plusNeg (PLUS xs) =
  PLUS $
    MultiSet.fold removeDuplicatesIteratively MultiSet.empty xs
  where
    removeDuplicatesIteratively x ys =
      case MultiSet.maxView $ duplicates x ys of
        Just (y, _) -> MultiSet.delete y ys
        Nothing -> MultiSet.insert x ys
    duplicates x ys =
      MultiSet.union
        (MultiSet.filter (== NEG x) ys)
        (MultiSet.filter ((== x) . NEG) ys)
_plusNeg x = x

_plusSingle :: Polynomial a -> Polynomial a
_plusSingle x@(PLUS xs)
  | MultiSet.size xs == 1 = MultiSet.findMin xs
  | otherwise = x
_plusSingle x = x

_combinePlus :: Ord a => Polynomial a -> Polynomial a
_combinePlus (PLUS xs) = PLUS $ foldr (MultiSet.union . childrenPlus) xs' pluses
  where
    childrenPlus (PLUS xs) = xs
    childrenPlus _ = error "Expected PLUS in _combinePlus"
    (pluses, xs') = MultiSet.partition isPlus xs
    isPlus (PLUS _) = True
    isPlus _ = False
_combinePlus x = x

_emptyTimes :: Polynomial a -> Polynomial a
_emptyTimes x@(TIMES xs)
  | MultiSet.null xs = ONE
  | otherwise = x
_emptyTimes x = x

_timesOne :: Eq a => Polynomial a -> Polynomial a
_timesOne (TIMES xs) = TIMES $ MultiSet.filter (/= ONE) xs
_timesOne x = x

_timesZero :: Eq a => Polynomial a -> Polynomial a
_timesZero x@(TIMES xs)
  | not . MultiSet.null $ MultiSet.filter (== ZERO) xs = ZERO
  | otherwise = x
_timesZero x = x

_timesNeg :: Ord a => Polynomial a -> Polynomial a
_timesNeg (TIMES xs)
  | even . MultiSet.size $ MultiSet.filter isNeg xs = TIMES allPos
  | otherwise = NEG $ TIMES allPos
  where
    allPos = MultiSet.map unNeg xs
    unNeg (NEG x) = x
    unNeg x = x
    isNeg (NEG _) = True
    isNeg _ = False
_timesNeg x = x

_timesSingle :: Polynomial a -> Polynomial a
_timesSingle x@(TIMES xs)
  | MultiSet.size xs == 1 = MultiSet.findMin xs
  | otherwise = x
_timesSingle x = x

_combineTimes :: Ord a => Polynomial a -> Polynomial a
_combineTimes (TIMES xs) =
  TIMES $ foldr (MultiSet.union . childrenTimes) xs' timeses
  where
    childrenTimes (TIMES xs) = xs
    childrenTimes _ = error "Expected TIMES in _combineTimes"
    (timeses, xs') = MultiSet.partition isTimes xs
    isTimes (TIMES _) = True
    isTimes _ = False
_combineTimes x = x

_distributive :: Ord a => Polynomial a -> Polynomial a
_distributive x@(TIMES xs)
  | any isPlus xs =
    PLUS
      . MultiSet.fromList
      . map TIMES
      $ flatMultiSetCartesianProductWith termsList xs
  | otherwise = x
  where
    termsList (PLUS ys) = MultiSet.toList ys
    termsList x = [x]
    isPlus (PLUS _) = True
    isPlus _ = False
_distributive x = x

flatMultiSetCartesianProductWith ::
  Ord a => (a -> [a]) -> MultiSet.MultiSet a -> [MultiSet.MultiSet a]
flatMultiSetCartesianProductWith f =
  map
    MultiSet.fromList
    . LS.cartesianProduct
    . map f
    . MultiSet.toList

---------- Interpreters ----------
data PolynomialRingsInterpreter a = PolynomialRingsInterpreter
  { operatorByID ::
      Map.Map
        a
        ([Polynomial a] -> Either EH.Error (Polynomial a)),
    isVariable :: a -> Bool
  }

instance Ord a => Interpreter.Interpreter (PolynomialRingsInterpreter a) a a where
  type LogicSystem (PolynomialRingsInterpreter a) = PolynomialRings
  operatorByID = operatorByID
  variableFromID i a
    | isVariable i a = VARIABLE a
    | otherwise = CONSTANT a

defaultPolynomialRingsInterpreter :: PolynomialRingsInterpreter String
defaultPolynomialRingsInterpreter =
  polynomialRingsInterpreterFromNames
    [ (zero, ["0", "ZERO", "Zero", "zero"]),
      (one, ["1", "ONE", "One", "one"]),
      (neg, ["-", "NEG", "Neg", "neg"]),
      (plus, ["+", "PLUS", "Plus", "plus"]),
      (times, ["*", "TIMES", "Times", "times"])
    ]
    firstIsUpper
  where
    firstIsUpper "" = False
    firstIsUpper (x : _) = Char.isUpper x

polynomialRingsInterpreterFromNames ::
  Ord a =>
  [ ( [Polynomial a] ->
      Either EH.Error (Polynomial a),
      [a]
    )
  ] ->
  (a -> Bool) ->
  PolynomialRingsInterpreter a
polynomialRingsInterpreterFromNames namesMap =
  PolynomialRingsInterpreter
    ( Map.fromList
        . concatMap (\(op, ns) -> map (,op) ns)
        $ namesMap
    )

zero :: [Polynomial a] -> Either EH.Error (Polynomial a)
zero = Interpreter.makeConstant ZERO "PolynomialRings.ZERO"

one :: [Polynomial a] -> Either EH.Error (Polynomial a)
one = Interpreter.makeConstant ONE "PolynomialRings.ONE"

neg :: [Polynomial a] -> Either EH.Error (Polynomial a)
neg = Interpreter.makeUnary NEG "PolynomialRings.NEG"

plus :: Ord a => [Polynomial a] -> Either EH.Error (Polynomial a)
plus = Interpreter.makeUnorderedVariadic PLUS "PolynomialRings.PLUS"

times :: Ord a => [Polynomial a] -> Either EH.Error (Polynomial a)
times = Interpreter.makeUnorderedVariadic TIMES "PolynomialRings.TIMES"
