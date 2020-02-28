module Language.Egison.Pretty.Pattern.Mode.Haskell.THSpec
  ( test_atom_patterns
  , test_pattern_operators
  )
where

import           TestImport

import           Data.Text                      ( pack )
import           Test.Tasty
import           Test.Tasty.HUnit


assertPrintExpr :: Expr Name Name Exp -> String -> Assertion
assertPrintExpr e expected = case testPrintExpr e of
  Left err -> fail $ show err
  Right got ->
    assertEqual ("while printing \"" ++ show e ++ "\"") (pack expected) got

name :: String -> Name
name = mkName

sym :: String -> Name
sym = mkName

intE :: Integer -> Exp
intE = LitE . IntegerL

{-# ANN infixApp "HLint: ignore Eta reduce" #-}
infixApp :: Exp -> Name -> Exp -> Exp
infixApp l op r = UInfixE l (VarE op) r

test_atom_patterns :: [TestTree]
test_atom_patterns =
  [ testCase "wildcard pattern" $ assertPrintExpr Wildcard "_"
  , testCase "variable pattern" $ assertPrintExpr (Variable $ name "x") "$x"
  , testCase "variable pattern of symbols"
    $ assertPrintExpr (Variable $ sym "%") "$(%)"
  , testCase "value pattern" $ assertPrintExpr (Value $ intE 10) "#10"
  , testCase "value pattern between parentheses"
    $ assertPrintExpr (Value $ infixApp (intE 1) (sym "+") (intE 2)) "#(1 + 2)"
  , testCase "predicate pattern" $ assertPrintExpr (Predicate $ intE 10) "?10"
  , testCase "constructor pattern" $ assertPrintExpr
    (Pattern (name "ctor") [Wildcard, Wildcard, Wildcard])
    "ctor _ _ _"
  , testCase "constructor pattern with data constructor" $ assertPrintExpr
    (Pattern (name "Ctor") [Wildcard, Wildcard, Wildcard])
    "Ctor _ _ _"
  , testCase "constructor pattern with qualified name" $ assertPrintExpr
    (Pattern (name "Mod.ctor") [Wildcard, Wildcard, Wildcard])
    "Mod.ctor _ _ _"
  , testCase "constructor pattern that the symbol is between parentheses"
    $ assertPrintExpr (Pattern (sym "++") [Wildcard, Wildcard]) "(++) _ _"
  , testCase
      "constructor pattern that the qualified symbol is between parentheses"
    $ assertPrintExpr (Pattern (sym "Mod.++") [Wildcard, Wildcard])
                      "(Mod.++) _ _"
  , testCase "constructor pattern without arguments"
    $ assertPrintExpr (Pattern (name "nil") []) "nil"
  , testCase "constructor pattern with qualified name without arguments"
    $ assertPrintExpr (Pattern (name "Mod.nil") []) "Mod.nil"
  ]

test_pattern_operators :: [TestTree]
test_pattern_operators =
  [ testCase "cons pattern"
    $ assertPrintExpr (Infix (sym ":") Wildcard Wildcard) "_ : _"
  , testCase "join pattern"
    $ assertPrintExpr (Infix (sym "++") Wildcard Wildcard) "_ ++ _"
  , testCase "nested cons pattern" $ assertPrintExpr
    (Infix (sym ":") Wildcard (Infix (sym ":") Wildcard Wildcard))
    "_ : _ : _"
  , testCase "nested cons pattern (with parentheses)" $ assertPrintExpr
    (Infix (sym ":") (Infix (sym ":") Wildcard Wildcard) Wildcard)
    "(_ : _) : _"
  , testCase "nested cons pattern (qualified, with parentheses)"
    $ assertPrintExpr
        (Infix (sym ":") (Infix (sym "Special.:") Wildcard Wildcard) Wildcard)
        "(_ Special.: _) : _"
  , testCase "nested join pattern" $ assertPrintExpr
    (Infix (sym "++") Wildcard (Infix (sym "++") Wildcard Wildcard))
    "_ ++ _ ++ _"
  , testCase "nested join pattern (with parentheses)" $ assertPrintExpr
    (Infix (sym "++") (Infix (sym "++") Wildcard Wildcard) Wildcard)
    "(_ ++ _) ++ _"
  , testCase "nested join, cons pattern" $ assertPrintExpr
    (Infix (sym "++") Wildcard (Infix (sym ":") Wildcard Wildcard))
    "_ ++ _ : _"
  , testCase "nested cons, join pattern" $ assertPrintExpr
    (Infix (sym ":") Wildcard (Infix (sym "++") Wildcard Wildcard))
    "_ : _ ++ _"
  , testCase "nested cons, join pattern (with parentheses)" $ assertPrintExpr
    (Infix (sym "++") (Infix (sym ":") Wildcard Wildcard) Wildcard)
    "(_ : _) ++ _"
  , testCase "non-symbol"
    $ assertPrintExpr (Infix (name "mod") Wildcard Wildcard) "_ `mod` _"
  , testCase "qualified non-symbol" $ assertPrintExpr
    (Infix (name "Special.mod") Wildcard Wildcard)
    "_ `Special.mod` _"
  , testCase "nested qualified non-symbol and symbol" $ assertPrintExpr
    (Infix (name "Special.:")
           Wildcard
           (Infix (name "Special.mod") Wildcard Wildcard)
    )
    "_ Special.: _ `Special.mod` _"
  ]

