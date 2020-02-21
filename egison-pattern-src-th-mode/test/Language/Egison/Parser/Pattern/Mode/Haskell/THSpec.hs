module Language.Egison.Parser.Pattern.Mode.Haskell.THSpec
  ( test_atom_patterns
  , test_pattern_operators
  , test_comments
  )
where

import           TestImport

import           Test.Tasty
import           Test.Tasty.HUnit


assertParseExpr :: String -> Expr Name Name Exp -> Assertion
assertParseExpr content expected = case testParseExpr content of
  Left  err -> fail $ show err
  Right got -> assertEqual ("while parsing \"" ++ content ++ "\"") expected got

name :: String -> Name
name = mkName

sym :: String -> Name
sym = mkName

intE :: Integer -> Exp
intE = LitE . IntegerL

{-# ANN infixApp "HLint: ignore Eta reduce" #-}
infixApp :: Exp -> Name -> Exp -> Exp
infixApp l op r = UInfixE l (VarE op) r

paren :: Exp -> Exp
paren = ParensE

test_atom_patterns :: [TestTree]
test_atom_patterns =
  [ testCase "wildcard pattern" $ assertParseExpr "_" Wildcard
  , testCase "variable pattern" $ assertParseExpr "$x" (Variable $ name "x")
  , testCase "variable pattern of symbols"
    $ assertParseExpr "$(%)" (Variable $ sym "%")
  , testCase "value pattern" $ assertParseExpr "#10" (Value $ intE 10)
  , testCase "value pattern between parentheses" $ assertParseExpr
    "#(1 + 2)"
    (Value . paren $ infixApp (intE 1) (sym "+") (intE 2))
  , testCase "predicate pattern" $ assertParseExpr "?10" (Predicate $ intE 10)
  , testCase "constructor pattern" $ assertParseExpr
    "(ctor _ _ _)"
    (Pattern (name "ctor") [Wildcard, Wildcard, Wildcard])
  , testCase "constructor pattern with data constructor" $ assertParseExpr
    "(Ctor _ _ _)"
    (Pattern (name "Ctor") [Wildcard, Wildcard, Wildcard])
  , testCase "constructor pattern with qualified name" $ assertParseExpr
    "(Mod.ctor _ _ _)"
    (Pattern (name "Mod.ctor") [Wildcard, Wildcard, Wildcard])
  , testCase "constructor pattern that the symbol is between parentheses"
    $ assertParseExpr "((++) _ _)" (Pattern (sym "++") [Wildcard, Wildcard])
  , testCase
      "constructor pattern that the qualified symbol is between parentheses"
    $ assertParseExpr "((Mod.++) _ _)"
                      (Pattern (sym "Mod.++") [Wildcard, Wildcard])
  , testCase "constructor pattern without arguments"
    $ assertParseExpr "nil" (Pattern (name "nil") [])
  , testCase "constructor pattern with qualified name without arguments"
    $ assertParseExpr "Mod.nil" (Pattern (name "Mod.nil") [])
  , testCase "constructor pattern between parentheses"
    $ assertParseExpr "((c _))" (Pattern (name "c") [Wildcard])
  , testCase "wildcard pattern between parentheses"
    $ assertParseExpr "((_))" Wildcard
  , testCase "constructor pattern without arguments between parentheses"
    $ assertParseExpr "((c))" (Pattern (name "c") [])
  , testCase
      "constructor pattern with qualified name without arguments between parentheses"
    $ assertParseExpr "((Mod.c))" (Pattern (name "Mod.c") [])
  ]

test_pattern_operators :: [TestTree]
test_pattern_operators =
  [ testCase "operator" $ assertParseExpr
      "_ && _ && _"
      (Infix (sym "&&") Wildcard (Infix (sym "&&") Wildcard Wildcard))
  ]

test_comments :: [TestTree]
test_comments =
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
