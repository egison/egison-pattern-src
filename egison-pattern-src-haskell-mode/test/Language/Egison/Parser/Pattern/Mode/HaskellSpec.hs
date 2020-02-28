module Language.Egison.Parser.Pattern.Mode.HaskellSpec
  ( test_atom_patterns
  , test_pattern_operators
  , test_special_pattern_operators
  , test_comments
  )
where

import           TestImport

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Functor                   ( void )
import           Language.Haskell.Exts.Build    ( name
                                                , sym
                                                , op
                                                , paren
                                                , infixApp
                                                , intE
                                                )


makeAssertion
  :: (String -> Either (Errors String) (Expr (QName ()) (Name ()) (Exp ())))
  -> String
  -> Expr (QName ()) (Name ()) (Exp ())
  -> Assertion
makeAssertion parser content expected = case parser content of
  Left  err -> fail $ show err
  Right got -> assertEqual ("while parsing \"" ++ content ++ "\"") expected got

assertParseExpr :: String -> Expr (QName ()) (Name ()) (Exp ()) -> Assertion
assertParseExpr = makeAssertion $ fmap (mapValueExpr void) . testParseExpr

assertParseExprSpecialFixities
  :: String -> Expr (QName ()) (Name ()) (Exp ()) -> Assertion
assertParseExprSpecialFixities =
  makeAssertion $ fmap (mapValueExpr void) . testParseExprSpecialFixities


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
  [ testCase "wildcard pattern" $ assertParseExpr "_" Wildcard
  , testCase "variable pattern" $ assertParseExpr "$x" (Variable $ name "x")
  , testCase "variable pattern of symbols"
    $ assertParseExpr "$(%)" (Variable $ sym "%")
  , testCase "value pattern" $ assertParseExpr "#10" (Value $ intE 10)
  , testCase "value pattern between parentheses" $ assertParseExpr
    "#(1 + 2)"
    (Value . paren $ infixApp (intE 1) (op $ sym "+") (intE 2))
  , testCase "predicate pattern" $ assertParseExpr "?10" (Predicate $ intE 10)
  , testCase "constructor pattern" $ assertParseExpr
    "ctor _ _ _"
    (Pattern (uqname "ctor") [Wildcard, Wildcard, Wildcard])
  , testCase "constructor pattern with data constructor" $ assertParseExpr
    "Ctor _ _ _"
    (Pattern (uqname "Ctor") [Wildcard, Wildcard, Wildcard])
  , testCase "constructor pattern with qualified name" $ assertParseExpr
    "Mod.ctor _ _ _"
    (Pattern (qname "Mod" "ctor") [Wildcard, Wildcard, Wildcard])
  , testCase "constructor pattern that the symbol is between parentheses"
    $ assertParseExpr "(++) _ _" (Pattern (uqsym "++") [Wildcard, Wildcard])
  , testCase
      "constructor pattern that the qualified symbol is between parentheses"
    $ assertParseExpr "(Mod.++) _ _"
                      (Pattern (qsym "Mod" "++") [Wildcard, Wildcard])
  , testCase "constructor pattern without arguments"
    $ assertParseExpr "nil" (Pattern (uqname "nil") [])
  , testCase "constructor pattern with qualified name without arguments"
    $ assertParseExpr "Mod.nil" (Pattern (qname "Mod" "nil") [])
  , testCase "constructor pattern between parentheses"
    $ assertParseExpr "((c _))" (Pattern (uqname "c") [Wildcard])
  , testCase "wildcard pattern between parentheses"
    $ assertParseExpr "((_))" Wildcard
  , testCase "constructor pattern without arguments between parentheses"
    $ assertParseExpr "((c))" (Pattern (uqname "c") [])
  , testCase
      "constructor pattern with qualified name without arguments between parentheses"
    $ assertParseExpr "((Mod.c))" (Pattern (qname "Mod" "c") [])
  ]

test_pattern_operators :: [TestTree]
test_pattern_operators =
  [ testCase "right associativity" $ assertParseExpr
    "_ && _ && _"
    (Infix (uqsym "&&") Wildcard (Infix (uqsym "&&") Wildcard Wildcard))
  , testCase "left associativity" $ assertParseExpr
    "_ >> _ >> _"
    (Infix (uqsym ">>") (Infix (uqsym ">>") Wildcard Wildcard) Wildcard)
  , testCase "precedence (right associativity)" $ assertParseExpr
    "_ . _ ^ _"
    (Infix (uqsym "^") (Infix (uqsym ".") Wildcard Wildcard) Wildcard)
  , testCase "precedence (left associativity)" $ assertParseExpr
    "_ * _ !! _"
    (Infix (uqsym "*") Wildcard (Infix (uqsym "!!") Wildcard Wildcard))
  , testCase "precedence (mixed)" $ assertParseExpr
    "_ $ _ >> _"
    (Infix (uqsym "$") Wildcard (Infix (uqsym ">>") Wildcard Wildcard))
  , testCase "non-symbol"
    $ assertParseExpr "_ `mod` _" (Infix (uqname "mod") Wildcard Wildcard)
  ]

test_special_pattern_operators :: [TestTree]
test_special_pattern_operators =
  [ testCase "cons pattern" $ assertParseExprSpecialFixities
    "_ : _"
    (Infix (uqsym ":") Wildcard Wildcard)
  , testCase "join pattern" $ assertParseExprSpecialFixities
    "_ ++ _"
    (Infix (uqsym "++") Wildcard Wildcard)
  , testCase "nested cons pattern" $ assertParseExprSpecialFixities
    "_ : _ : _"
    (Infix (uqsym ":") Wildcard (Infix (uqsym ":") Wildcard Wildcard))
  , testCase "nested join pattern" $ assertParseExprSpecialFixities
    "_ ++ _ ++ _"
    (Infix (uqsym "++") Wildcard (Infix (uqsym "++") Wildcard Wildcard))
  , testCase "nested join, cons pattern" $ assertParseExprSpecialFixities
    "_ ++ _ : _"
    (Infix (uqsym "++") Wildcard (Infix (uqsym ":") Wildcard Wildcard))
  , testCase "nested cons, join pattern" $ assertParseExprSpecialFixities
    "_ : _ ++ _"
    (Infix (uqsym ":") Wildcard (Infix (uqsym "++") Wildcard Wildcard))
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
