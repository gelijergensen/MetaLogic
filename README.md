# MetaLogic
![status badge](https://github.com/gelijergensen/MetaLogic/actions/workflows/haskell.yml/badge.svg)

A simple calculator for general logic systems.

This project is part of my Haskell portfolio, which can be found [here](https://github.com/gelijergensen/Haskell-Portfolio).

## Project Structure

The project consists of a library and an executable which depends on the library.
All code for the library is found in the `src/` directory.
- `src/Main` is simply a few functions for ease of use in ghci,
- `src/ClassicalPropositionalLogic`, `src/IntuitionisticPropositionalLogic`, `src/PeanoArithmetic`, and `src/PolynomialRings` each define a single logic system,
- `src/LogicSystem` holds the abstract definition of a logic system and rewriting,
- `src/AbstractSyntaxTree` and `src/Interpreter` handle the internals of representing formulas which have been parsed but not yet passed on to a logic system,
- `src/ErrorHandling` does what it says on the tin,
- `src/Parser` contains the parser combinators for parsing formulas from a string, and
- `src/RApplicative` holds a (partial) definition for a constrained applicative typeclass.

The code for the executable is in the `exe/` directory.
- `exe/Main` handles the control flow of the calculator and
- `exe/ReplParser` contains the parser combinators for parsing user input.

The `test/` directory contains tests for the library modules.

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
exampleSystem1 = ExampleLogicSystem $ map ExampleRule [notTrueIsFalse, notFalseIsTrue]

-- variables named by integers (and there aren't any rewrite rules)
exampleSystem2 :: ExampleLogicSystem Int
exampleSystem2 = ExampleLogicSystem $ map ExampleRule []
```

# Examples of Logic Systems

Next we briefly describe each of the four logic systems which are implemented in this project.

## Classical Propositional Logic

The canonical example of a logic system is classical propositional logic.
Formulas and rules are defined very similarly to the example above:
```haskell
data PropFormula a
  = TRUE
  | FALSE
  | VAR a
  | NOT (PropFormula a)
  | OR (PropFormula a) (PropFormula a)
  | AND (PropFormula a) (PropFormula a)
  | IMPLIES (PropFormula a) (PropFormula a)
  deriving (Eq, Ord, Show)

newtype PropRule a = PropRule
  { runRule ::
      PropFormula a ->
      PropFormula a
  }
```
The actual rewrite rules are then the typical logical equivalences, e.g. `AND a b = AND b a`, `OR (NOT a) a = TRUE`, or `NOT FALSE = TRUE`.
In order to ensure that the rewriting process terminates, each rewrite rule attempts to simplify the formula, e.g. the logical equivalence `NOT FALSE = TRUE` would correspond to the single rule
```haskell
notFalseIsTrue = 
  PropRule
    ( \case
        NOT FALSE -> TRUE
        x -> x
    )
```
since the formula `TRUE` is simpler than the formula `NOT FALSE`.
Lastly, we define `IMPLIES a b` to be `OR (NOT a) b` (thereby eliminating the case of implication for all other rewrite rules), but we do not consider `OR` or `AND` to be defined in terms of the other (by the de Morgan's laws), as that makes the resulting "simplified" formulas harder to read.

## Intuitionistic Propositional Logic

Whereas classical propositional logic can be thought of as the "calculus of true/false" statements, intuitionistic propositional logic can be thought of as the "calculus of construction/destruction processes".
Formally, intuitionistic propositional logic is the logical system that results when we remove the law of excluded middle from classical propositional logic.
In other words, we no longer have the rewrite rule `OR (NOT a) a -> TRUE`.
Similarly, we also don't have the rule `NOT FALSE -> TRUE`.
Notice that the reverse directions are still valid: out of the truth of something, one can still conclude that it is not false; it just happens that the second statement is strictly weaker in intuitionistic logic.

Typically, we think of intuitionistic logic as a form of constructive logic: each logical proof is strong enough to be directly turned into an algorithm which computes the result.
For instance, say I want to show that every natural number is either zero or nonzero.
Classically, this is tautological: either a number is zero or it is not zero by the law of excluded middle.
Intuitionistically, I can't use the law of excluded middle like this, so I have to offer a proof which uses the properties of natural numbers (for example using the Peano axioms):
> Every natural number `x` is either zero or is the successor of a natural number (this is the definition, an axiom).
> If `x` is zero, then it is zero. 
> In particular, it is either zero or nonzero.
> If `x` is the successor of a natural number, then it is not zero (this is another axiom).
> In particular, it is either zero or nonzero.

Notice that this proof contains an algorithm which computes, for any given integer, whether it is zero or whether it is not zero.
It would correspond to the following Haskell code:
```haskell
data Natural 
  = Zero
  | Succ Natural

isZero :: Natural -> Bool
isZero Zero = True
isZero (Succ _) = False
```
Naturally, this is obvious in this simple case, but for more complex statements, this is not clear _a priori_.

To actually implement this in Haskell, we can completely reuse the definitions of `PropFormula` and `PropRule` from above and only change which rewrite rules are available.
In this project, we defined `ClassicalPropositionalLogic` and `IntuitionisticPropositionalLogic` independently, partially to make them more conceptually distinct and partially to ensure that parsing and interpreting formulas in these logic systems is independent.

In our classical propositional logic implementation, `IMPLIES a b` was simply a shorthand for `OR (NOT a) b`.
In our intuitionistic propositional logic implementation, we take the opposite approach: `IMPLIES a b` is considered as one of the simplest statements and `NOT a` is defined as shorthand for `IMPLIES a FALSE`.
This is why intuitionistic propositional logic can be thought of as a "calculus of construction/destruction processes":
if we interpret `IMPLIES a b` as meaning "out of `a` I can construct `b`", `TRUE` as meaning "everything", and `FALSE` as meaning "nothing", then `IMPLIES TRUE a` means "I can construct `a` from everything", or, more simply, "I can construct `a`".
On the other hand `IMPLIES a FALSE` means "I can construct nothing from `a`", or alternatively "I can destroy `a`".
Lastly, if a particular logical framework is inconsistent (i.e. one can prove `IMPLIES TRUE FALSE`), then that means "I can destroy everything/anything".

The rewrite rules of `IntuitionisticPropositionalLogic` are all of those of `ClassicalPropositionalLogic` (except for those derived from the law of excluded middle) along with rules for handling implications explicitly.

## Peano Arithmetic

Peano Arithmetic offers a particular formalization of the arithmetic of natural numbers.
Every natural number is either zero or the successor of a natural number (`S(n)`) and we can also add and multiply natural numbers.
In Haskell, we might define
```haskell
data PeanoFormula a
  = ZERO
  | VAR a
  | S (PeanoFormula a)
  | PLUS (PeanoFormula a) (PeanoFormula a)
  | TIMES (PeanoFormula a) (PeanoFormula a)
  deriving (Eq, Ord, Show)

newtype PeanoRule a = PeanoRule
  { runRule ::
      PeanoFormula a ->
      PeanoFormula a
  }
```
In our implementation, the rewrite system works to simplify the representation of a number.
There are surprisingly few rules:
- `PLUS x ZERO -> x`
- `PLUS x (S y) -> S (PLUS x y)`
- `TIMES x ZERO -> ZERO`
- `TIMES x (S y) -> PLUS x (TIMES x y)`

along with rules for the associativity and commutativity of addition and multiplication.
As such, rewriting a formula (without variables) is equivalent to computing the resulting natural number.
For instance `rewrite (TIMES (PLUS (S ZERO) (S (S ZERO)) (S (S ZERO))))` would yield `S (S (S (S (S (S ZERO)))))`, since `(1 + 2) * 2 = 6`.

## Polynomial Rings

For our last example, we present a "logic system" which is really just a term rewrite system: rearranging polynomials so that they are in normal form.
We take the view that a term in a polynomial is either a constant or a variable, possibly negated, and that a polynomial is a sum or a product of polynomials.
In Haskell code, we have
```haskell
data Polynomial a
  = ZERO
  | ONE
  | CONSTANT a
  | VARIABLE a
  | NEG (Polynomial a)
  | PLUS (MultiSet.MultiSet (Polynomial a))
  | TIMES (MultiSet.MultiSet (Polynomial a))
```
where `ZERO` and `ONE` are defined mostly for convenience.
We represent sums and products as multisets of polynomials, which allows us to use the count of an item for pretty-printing:
`TIMES X X` can be printed as `X^2` and `PLUS X X` can be printed as `2*X`.
Notice that both constants and variables have the same type.
The reason for this is that we wish to allow for irrational or even transcendental constants in the polynomials, which can only be accomplished (syntactically), if we name them.
It is easy to ensure that the names of constants and variables have different types (if desired) by simply using a sum type for the type variable `a`.

The rewrite rules of `PolynomialRings` are centered around eliminating sums and products (e.g. when multiplying by zero), adding and multiplying negative terms, and distributing negation before summation before multiplication.
In other words, the rewrite rules ensure that `* (+ X Y) (+ X Y) (+ Z Z) (+ 2 (- 3))` is reduced to `(2 - 3)*(X + Y)^2*(2*Z)`.

# Calculator executable

As part of this project, we include an interactive calculator for reducing formulas of each of the 4 implemented logic systems. 
If you use cabal, the executable can be run with a simple `cabal run metalogic-calc`.
The calculator allows you to input formulas at the command prompt and interprets the strings using pre-defined defaults (see the default `Interpreter`s in each of the modules for the individual logic systems). 
In order to prevent ambiguity, we parse strings using prefix notation.
Once formulas have been entered, you can choose to either perform a single step of rewriting or rewriting all at once.
For each of the 4 logic systems, we have ensured that all rewrite rules do not increase the complexity of a formula, so the rewriting process is guaranteed to terminate (although it is an exponential algorithm in general).

Here is an example of using the calculator to simplify a propositional logic statement in both classical and intuitionistic logic.
```sh-session
Welcome to the MetaLogic Calculator!
You may quit at any time by typing ":quit". For help, type ":help".

What logic system would you like to work with today?
Classical Propositional Logic (CPL)
Intuitionistic Propositional Logic (IPL)
Peano Arithmetic (PA)
Polynomial Rings (PR)

$> CPL
Classical Propositional Logic selected. You may change this at any time by typing ":set system" followed by a logic system name.

Please enter a formula. You may name formulas for later reference by writing <name> = <formula>. For example, "x = TRUE".
Once a formula has been entered, you may perform a single rewrite step on it by typing ":step <name>" or a complete rewrite by typing ":rewrite <name>". If no name is given, the last unnamed formula will be used.
To list available formulas, type ":list" or ":l" at any time.
$> x = OR (OR X (NOT X)) (AND Y (NOT Y))

$> :list
x = OR (OR (VAR "X") (NOT (VAR "X"))) (AND (VAR "Y") (NOT (VAR "Y")))

$> :step x
x_0 = OR (AND (VAR "Y") (NOT (VAR "Y"))) (OR (VAR "X") (NOT (VAR "X")))
x_1 = OR (OR (NOT (VAR "X")) (VAR "X")) (AND (NOT (VAR "Y")) (VAR "Y"))
x_2 = OR (OR (NOT (VAR "X")) (VAR "X")) (AND (VAR "Y") (NOT (VAR "Y")))
x_3 = OR (OR (NOT (VAR "X")) (VAR "X")) FALSE
x_4 = OR (OR (VAR "X") (NOT (VAR "X"))) (AND (NOT (VAR "Y")) (VAR "Y"))
x_5 = OR (OR (VAR "X") (NOT (VAR "X"))) (AND (VAR "Y") (NOT (VAR "Y")))
x_6 = OR (OR (VAR "X") (NOT (VAR "X"))) FALSE
x_7 = OR (VAR "X") (OR (NOT (VAR "X")) (AND (VAR "Y") (NOT (VAR "Y"))))
x_8 = OR TRUE (AND (NOT (VAR "Y")) (VAR "Y"))
x_9 = OR TRUE (AND (VAR "Y") (NOT (VAR "Y")))
x_10 = OR TRUE FALSE

$> :rewrite x
x_0 = OR (AND (NOT (VAR "Y")) (VAR "Y")) (OR (NOT (VAR "X")) (VAR "X"))
x_1 = OR (AND (NOT (VAR "Y")) (VAR "Y")) (OR (VAR "X") (NOT (VAR "X")))
x_2 = OR (AND (NOT (VAR "Y")) (VAR "Y")) TRUE
...
x_41 = OR TRUE (AND (NOT (VAR "Y")) (VAR "Y"))
x_42 = OR TRUE (AND (VAR "Y") (NOT (VAR "Y")))
x_43 = OR TRUE FALSE
x_44 = TRUE

$> :set system IPL
Switched to Intuitionistic Propositional Logic.
$> :list

$> x = OR (OR X (NOT X)) (AND Y (NOT Y))

$> :rewrite x
x_0 = OR (NOT (VAR "X")) (OR (AND (NOT (VAR "Y")) (VAR "Y")) (VAR "X"))
x_1 = OR (NOT (VAR "X")) (OR (AND (IMPLIES (VAR "Y") FALSE) (VAR "Y")) (VAR "X"))
x_2 = OR (NOT (VAR "X")) (OR (AND (VAR "Y") (NOT (VAR "Y"))) (VAR "X"))
...
x_97 = OR (OR FALSE (VAR "X")) (IMPLIES (VAR "X") FALSE)
x_98 = OR (VAR "X") (NOT (VAR "X"))
x_99 = OR (VAR "X") (IMPLIES (VAR "X") FALSE)
x_100 = OR (VAR "X") (OR (NOT (VAR "X")) (AND (NOT (VAR "Y")) (VAR "Y")))
x_101 = OR (VAR "X") (OR (NOT (VAR "X")) (AND (IMPLIES (VAR "Y") FALSE) (VAR "Y")))
...
x_121 = OR FALSE (OR (IMPLIES (VAR "X") FALSE) (VAR "X"))
x_122 = OR FALSE (OR (VAR "X") (NOT (VAR "X")))
x_123 = OR FALSE (OR (VAR "X") (IMPLIES (VAR "X") FALSE))

$> :rewrite x_99
x_99_0 = OR (IMPLIES (VAR "X") FALSE) (VAR "X")
x_99_1 = OR (VAR "X") (IMPLIES (VAR "X") FALSE)

$> :q
Goodbye!
```
Notice that the intuitive "simplicity" of a formula doesn't quite match the order in which formulas are printed, so one might have to search the list of formulas to find the simplest one (such as `x_99` in the second example).

# References

[1]: Baader, Franz, and Nipkow, Tobias. Term rewriting and all that. Cambridge university press, 1999.
