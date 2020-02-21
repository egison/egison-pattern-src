module Language.Egison.Pretty.Pattern.Mode.HaskellSpec
  ( test_atom_patterns
  , test_special_pattern_operators
  )
where

import           TestImport

import           Data.Text                      ( pack )
import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.Haskell.Exts.Build    ( name
                                                , sym
                                                , op
                                                , infixApp
                                                , intE
                                                )


assertPrintExpr :: Expr (QName ()) (Name ()) (Exp ()) -> String -> Assertion
assertPrintExpr e expected = case testPrintExpr e of
  Left err -> fail $ show err
  Right got ->
    assertEqual ("while printing \"" ++ show e ++ "\"") (pack expected) got

uqname :: String -> QName ()
uqname = UnQual () . name

qname :: String -> String -> QName ()
qname modName = Qual () (ModuleName () modName) . name

uqsym :: String -> QName ()
uqsym = UnQual () . sym

qsym :: String -> String -> QName ()
qsym modName = Qual () (ModuleName () modName) . sym

test_atom_patterns :: [TestTree]
test_atom_patterns =
  [ testCase "wildcard pattern" $ assertPrintExpr Wildcard "_"
  , testCase "variable pattern" $ assertPrintExpr (Variable $ name "x") "$x"
  , testCase "variable pattern of symbols"
    $ assertPrintExpr (Variable $ sym "%") "$(%)"
  , testCase "value pattern" $ assertPrintExpr (Value $ intE 10) "#10"
  , testCase "value pattern between parentheses" $ assertPrintExpr
    (Value $ infixApp (intE 1) (op $ sym "+") (intE 2))
    "#(1 + 2)"
  , testCase "predicate pattern" $ assertPrintExpr (Predicate $ intE 10) "?10"
  , testCase "constructor pattern" $ assertPrintExpr
    (Pattern (uqname "ctor") [Wildcard, Wildcard, Wildcard])
    "(ctor _ _ _)"
  , testCase "constructor pattern with data constructor" $ assertPrintExpr
    (Pattern (uqname "Ctor") [Wildcard, Wildcard, Wildcard])
    "(Ctor _ _ _)"
  , testCase "constructor pattern with qualified name" $ assertPrintExpr
    (Pattern (qname "Mod" "ctor") [Wildcard, Wildcard, Wildcard])
    "(Mod.ctor _ _ _)"
  , testCase "constructor pattern that the symbol is between parentheses"
    $ assertPrintExpr (Pattern (uqsym "++") [Wildcard, Wildcard]) "((++) _ _)"
  , testCase
      "constructor pattern that the qualified symbol is between parentheses"
    $ assertPrintExpr (Pattern (qsym "Mod" "++") [Wildcard, Wildcard])
                      "((Mod.++) _ _)"
  , testCase "constructor pattern without arguments"
    $ assertPrintExpr (Pattern (uqname "nil") []) "nil"
  , testCase "constructor pattern with qualified name without arguments"
    $ assertPrintExpr (Pattern (qname "Mod" "nil") []) "Mod.nil"
  ]

test_special_pattern_operators :: [TestTree]
test_special_pattern_operators =
  [ testCase "cons pattern"
    $ assertPrintExpr (Infix (uqsym ":") Wildcard Wildcard) "_ : _"
  , testCase "join pattern"
    $ assertPrintExpr (Infix (uqsym "++") Wildcard Wildcard) "_ ++ _"
  , testCase "nested cons pattern" $ assertPrintExpr
    (Infix (uqsym ":") Wildcard (Infix (uqsym ":") Wildcard Wildcard))
    "_ : _ : _"
  , testCase "nested cons pattern (with parentheses)" $ assertPrintExpr
    (Infix (uqsym ":") (Infix (uqsym ":") Wildcard Wildcard) Wildcard)
    "(_ : _) : _"
  , testCase "nested cons pattern (qualified, with parentheses)"
    $ assertPrintExpr
        (Infix (uqsym ":")
               (Infix (qsym "Special" ":") Wildcard Wildcard)
               Wildcard
        )
        "(_ Special.: _) : _"
  , testCase "nested join pattern" $ assertPrintExpr
    (Infix (uqsym "++") Wildcard (Infix (uqsym "++") Wildcard Wildcard))
    "_ ++ _ ++ _"
  , testCase "nested join pattern (with parentheses)" $ assertPrintExpr
    (Infix (uqsym "++") (Infix (uqsym "++") Wildcard Wildcard) Wildcard)
    "(_ ++ _) ++ _"
  , testCase "nested join, cons pattern" $ assertPrintExpr
    (Infix (uqsym "++") Wildcard (Infix (uqsym ":") Wildcard Wildcard))
    "_ ++ _ : _"
  , testCase "nested cons, join pattern" $ assertPrintExpr
    (Infix (uqsym ":") Wildcard (Infix (uqsym "++") Wildcard Wildcard))
    "_ : _ ++ _"
  , testCase "nested cons, join pattern (with parentheses)" $ assertPrintExpr
    (Infix (uqsym "++") (Infix (uqsym ":") Wildcard Wildcard) Wildcard)
    "(_ : _) ++ _"
  , testCase "qualified non-symbol" $ assertPrintExpr
    (Infix (qname "Special" "mod") Wildcard Wildcard)
    "_ `Special.mod` _"
  , testCase "nested qualified non-symbol and symbol" $ assertPrintExpr
    (Infix (qsym "Special" ":")
           Wildcard
           (Infix (qname "Special" "mod") Wildcard Wildcard)
    )
    "_ Special.: _ `Special.mod` _"
  ]

