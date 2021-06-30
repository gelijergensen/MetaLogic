# MetaLogic
![status badge](https://github.com/gelijergensen/MetaLogic/actions/workflows/haskell.yml/badge.svg)

A simple calculator for general logic systems.

This project is part of my Haskell portfolio, which can be found [here](https://github.com/gelijergensen/Haskell-Portfolio).

# Logic Systems

We first briefly describe the formalization of logic systems and the general implementation here.
In the following section, we also summarize the application to a few particular logic systems.
In the last section, we show some examples of using the executable to rewrite individual logical formulas.

A logic system (also known as a formal system or an axiomatic system) is a structure which can be used to derive theorems from a collection of axioms.
This can be thought of as consisting primarily of two pieces of data
1. A collection of axioms (or, more generally, axiom schema)
2. A set of logical rules for deriving new statements from known ones

Here, we take a slightly broader viewpoint and consider all structures which define a collection of "syntactically valid" statements along with rules for rewriting these statements into other (equivalent) ones.
In the literature, such a structure is usually called a "term rewriting system" (see, for example [[1]](#References)).
As most of the applications in this project are axiomatic systems, we refer to them as logic systems in the code.
In Haskell, we might define a datatype for a (propositional) logic system like so:
```haskell
data Formula a
  = TRUE
  | FALSE
  | VAR a
  | NOT (Formula a)
  | AND (Formula a) (Formula a)

type Rule a = Formula a -> Formula a

newtype LogicSystem a
  = LogicSystem {
    rewriteRules: [Rule a]
  }
```
and a particular instance of this logic system might look something like
```haskell
exampleSystem :: LogicSystem a
exampleSystem = LogicSystem [notTrueIsFalse, notFalseIsTrue]

notTrueIsFalse :: Formula a -> Formula a
notTrueIsFalse (NOT TRUE) = FALSE
notTrueIsFalse x = x

notFalseIsTrue :: Formula a -> Formula a
notFalseIsTrue (NOT FALSE) = TRUE
notFalseIsTrue x = x
```
Rewriting a formula is simply a matter of iteratively applying all possible rules.
We do have to remember that a rule can be applied to a particular part of a formula (e.g. we can apply `notTrueIsFalse` to the formula `NOT (NOT TRUE)`), so we'll split the process of applying a rule to a formula from that of applying all rules.
```haskell
applyRule :: Rule a -> Formula a -> Set (Formula a)
applyRule r x@(AND a b) = singleton (r x) `union` liftA2 AND (applyRule a) (applyRule b)
applyRule r x@(NOT a) = singleton (r x) `union` fmap NOT (applyRule a)
applyRule r x = singleton (r x)

rewriteOnce :: LogicSystem a -> Formula a -> Set (Formula a)
rewriteOnce sys x = unions $ map (`applyRule` x) (rewriteRules sys)
```
Applying all rules iteratively can either be done a preset number of times or until the application of rules no longer yields a new formula.
In both cases, we can think of this process as doing a search on a graph of equivalent formulas, where the rewrite rules define which formulas are neighbors.
In this project, we simply use a breadth-first search, which is sufficient for small formulas.
For larger formulas, one would need to use some heuristic search, at which point issues of confluence and termination come into play.
See [[1]](#References) for more details.

While the above is a nice description for a single logic system, it quickly runs into problems when one wishes to handle multiple logic systems.
Primarily, because syntactically valid formulas can differ between logic systems, we need a type for the formulas of each logic system.
This also requires that the logic systems each be different types.
Despite this, the process of rewriting is roughly the same for each logic system, which motivates the use of typeclasses.
In particular, we take advantage of the [TypeFamilies](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html#) extension.
```haskell
class LogicSystem t a where
  data Formula t a
  data Rule t a 

  rewriteRules :: t -> [Rule t a]
  applyRule :: Rule t a -> Formula t a -> Set (Formula t a)
```
Every logic system has an associated type for the formulas, which implicitly defines the collection of syntactically valid formulas for that system.
We also allow the user to define an associated type for the rules.
This gives the flexibility for rules to contain other information (such as context).
We can upgrade the above example logic system like so:
```haskell
data ExampleFormula a 
  = TRUE
  | FALSE
  | VAR a
  | NOT (ExampleFormula a)
  | AND (ExampleFormula a) (ExampleFormula a)

newtype ExampleRule a = ExampleRule 
  { runRule :: ExampleFormula a -> ExampleFormula a
  }

newtype ExampleLogicSystem a = ExampleLogicSystem 
  { exampleRewriteRules :: [ExampleRule a]
  }

instance LogicSystem (ExampleLogicSystem a) a where
  data Formula (ExampleLogicSystem a) a = ExampleFormula a
  data Rule (ExampleLogicSystem a) a = ExampleRule a

  rewriteRules = exampleRewriteRules
  applyRule r x@(AND a b) = singleton (runRule r x) `union` liftA2 AND (applyRule a) (applyRule b)
  applyRule r x@(NOT a) = singleton (runRule r x) `union` fmap NOT (applyRule a)
  applyRule r x = singleton (runRule r x)
```
and we can actually define a particular instance (or two) of this logic system
```haskell
-- variables named by chars
exampleSystem1 :: ExampleLogicSystem Char
exampleSystem1 = ExampleLogicSystem [notTrueIsFalse, notFalseIsTrue]

-- variables named by integers (and there aren't any rewrite rules)
exampleSystem2 :: ExampleLogicSystem Int
exampleSystem2 = ExampleLogicSystem []
```

# References

[1]: Baader, Franz, and Nipkow, Tobias. Term rewriting and all that. Cambridge university press, 1999.
