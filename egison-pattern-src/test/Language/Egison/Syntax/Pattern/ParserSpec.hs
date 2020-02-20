module Language.Egison.Syntax.Pattern.ParserSpec
  ( test_atom_patterns
  , test_primitive_pattern_operators
  , test_user_defined_pattern_operators
  , test_user_defined_comments
  )
where

import           TestImport

import           Test.Tasty
import           Test.Tasty.HUnit


assertParseExpr :: String -> Expr Name Name ValueExpr -> Assertion
assertParseExpr content expected = case testParseExpr content of
  Left  err -> fail $ show err
  Right got -> assertEqual ("while parsing \"" ++ content ++ "\"") expected got

test_atom_patterns :: [TestTree]
test_atom_patterns =
  [ testCase "wildcard pattern" $ assertParseExpr "_" Wildcard
  , testCase "variable pattern" $ assertParseExpr "$x" (Variable $ Name "x")
  , testCase "value pattern" $ assertParseExpr "#10" (Value $ ValueExprInt 10)
  , testCase "value pattern between parentheses"
    $ assertParseExpr "#(-10)" (Value . ValueExprInt $ -10)
  , testCase "predicate pattern"
    $ assertParseExpr "?10" (Predicate $ ValueExprInt 10)
  , testCase "constructor pattern" $ assertParseExpr
    "(ctor _ _ _)"
    (Pattern (Name "ctor") [Wildcard, Wildcard, Wildcard])
  , testCase "constructor pattern that the name is between parentheses"
    $ assertParseExpr "((++) _ _)" (Pattern (Name "++") [Wildcard, Wildcard])
  , testCase "constructor pattern without arguments"
    $ assertParseExpr "nil" (Pattern (Name "nil") [])
  , testCase "constructor pattern between parentheses"
    $ assertParseExpr "((c _))" (Pattern (Name "c") [Wildcard])
  , testCase "wildcard pattern between parentheses"
    $ assertParseExpr "((_))" Wildcard
  , testCase "constructor pattern without arguments between parentheses"
    $ assertParseExpr "((c))" (Pattern (Name "c") [])
  , testCase "nested constructor pattern" $ assertParseExpr
    "(ctorA (ctorB _) (ctorC _ (ctorD _)) _ ctorE)"
    (Pattern
      (Name "ctorA")
      [ Pattern (Name "ctorB") [Wildcard]
      , Pattern (Name "ctorC") [Wildcard, Pattern (Name "ctorD") [Wildcard]]
      , Wildcard
      , Pattern (Name "ctorE") []
      ]
    )
  ]

test_primitive_pattern_operators :: [TestTree]
test_primitive_pattern_operators =
  [ testCase "and pattern" $ assertParseExpr "_ & _" (And Wildcard Wildcard)
  , testCase "or pattern" $ assertParseExpr "_ | _" (Or Wildcard Wildcard)
  , testCase "not pattern" $ assertParseExpr "!_" (Not Wildcard)
  -- associativity
  , testCase "nested and pattern"
    $ assertParseExpr "_ & _ & _" (And Wildcard (And Wildcard Wildcard))
  , testCase "nested or pattern"
    $ assertParseExpr "_ | _ | _" (Or Wildcard (Or Wildcard Wildcard))
  -- precedence
  , testCase "nested and, or pattern" $ assertParseExpr
    "_ & _ | _ & _"
    (Or (And Wildcard Wildcard) (And Wildcard Wildcard))
  , testCase "nested not, or pattern"
    $ assertParseExpr "! _ | _" (Or (Not Wildcard) Wildcard)
  , testCase "nested not, and pattern"
    $ assertParseExpr "! _ & _" (And (Not Wildcard) Wildcard)
  ]

test_user_defined_pattern_operators :: [TestTree]
test_user_defined_pattern_operators =
  [ testCase "cons pattern"
    $ assertParseExpr "_ : _" (Infix (Name ":") Wildcard Wildcard)
  , testCase "join pattern"
    $ assertParseExpr "_ ++ _" (Infix (Name "++") Wildcard Wildcard)
  -- associativity
  , testCase "nested cons pattern" $ assertParseExpr
    "_ : _ : _"
    (Infix (Name ":") Wildcard (Infix (Name ":") Wildcard Wildcard))
  , testCase "nested join pattern" $ assertParseExpr
    "_ ++ _ ++ _"
    (Infix (Name "++") Wildcard (Infix (Name "++") Wildcard Wildcard))
  , testCase "nested join, cons pattern" $ assertParseExpr
    "_ ++ _ : _"
    (Infix (Name "++") Wildcard (Infix (Name ":") Wildcard Wildcard))
  , testCase "nested cons, join pattern" $ assertParseExpr
    "_ : _ ++ _"
    (Infix (Name ":") Wildcard (Infix (Name "++") Wildcard Wildcard))
  -- and more
  , testCase "precedence" $ assertParseExpr
    "_ : _ <| _"
    (Infix (Name "<|") (Infix (Name ":") Wildcard Wildcard) Wildcard)
  , testCase "associativity" $ assertParseExpr
    "_ |> _ |> _"
    (Infix (Name "|>") (Infix (Name "|>") Wildcard Wildcard) Wildcard)
  ]

test_user_defined_comments :: [TestTree]
test_user_defined_comments =
  [ testCase "ignore a block comment"
    $ assertParseExpr "_ {- comment -} & _" (And Wildcard Wildcard)
  , testCase "ignore block comments" $ assertParseExpr
    "_ {- comment1 -}{- comment2 -} & (_ &{- comment3 -}_)"
    (And Wildcard (And Wildcard Wildcard))
  , testCase "ignore a block comment at beginning of line"
    $ assertParseExpr "{- comment -}_" Wildcard
  , testCase "ignore a line comment"
    $ assertParseExpr "_-- comment -- yeah" Wildcard
  , testCase "ignore a line comment at beginning of line"
    $ assertParseExpr "-- comment -- yeah \n_-- comment" Wildcard
  ]
