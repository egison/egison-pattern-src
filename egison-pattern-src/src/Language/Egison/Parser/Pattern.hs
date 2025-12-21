{-# OPTIONS_GHC -Wno-orphans #-}

-- |
--
-- Module:      Language.Egison.Parser.Pattern
-- Description: Parser for Egison patterns
-- Stability:   experimental
--
-- A parser for Egison patterns.
module Language.Egison.Parser.Pattern
  ( parseExpr,
    parseExprL,
    module X,
  )
where

-- re-exports

-- main

import Control.Applicative ((<|>))
import Control.Comonad.Cofree (unwrap)
import Control.Monad.Combinators
  ( many,
    sepBy,
  )
import Control.Monad.Except (MonadError)
import Language.Egison.Parser.Pattern.Combinator
  ( parens,
    token,
  )
import Language.Egison.Parser.Pattern.Expr
  ( Table (..),
    addInfix,
    atomParser,
    exprParser,
    initTable,
  )
import Language.Egison.Parser.Pattern.Expr as X
  ( Associativity (..),
    ExprL,
    Fixity (..),
    Precedence (..),
  )
import Language.Egison.Parser.Pattern.Parsable as X
  ( Parsable (..),
  )
import Language.Egison.Parser.Pattern.Prim
  ( Parse,
    lexeme,
    name,
    runParse,
    space,
    valueExpr,
    varName,
    (<?>),
  )
import Language.Egison.Parser.Pattern.Prim as X
  ( Error (..),
    ErrorItem (..),
    Errors,
    ExtParser,
    Location (..),
    ParseFixity (..),
    ParseMode (..),
    Position (..),
    Source,
    Token,
    Tokens,
  )
import Language.Egison.Parser.Pattern.Token as X
  ( IsToken (..),
  )
import Language.Egison.Parser.Pattern.Token qualified as Token
  ( IsToken (..),
  )
import Language.Egison.Syntax.Pattern.Base
  ( ExprF (..),
  )
import Language.Egison.Syntax.Pattern.Expr
  ( Expr,
  )
import Language.Egison.Syntax.Pattern.Fixity.Primitive qualified as PrimOp

primInfixes ::
  (Source s) =>
  [(Precedence, Table (Parse n v e s) (ExprF n v e) (ExprL n v e))]
primInfixes =
  [ ( PrimOp.andPrecedence,
      addInfix PrimOp.andAssociativity (AndF <$ token Token.and) initTable
    ),
    ( PrimOp.orPrecedence,
      addInfix PrimOp.orAssociativity (OrF <$ token Token.vertical) initTable
    )
  ]

wildcard :: (Source s) => Parse n v e s (ExprF n v e a)
wildcard = WildcardF <$ token Token.underscore

variable :: (Source s) => Parse n v e s (ExprF n v e a)
variable = do
  token Token.dollar
  v <- lexeme varName
  pure $ VariableF v

value :: (Source s) => Parse n v e s (ExprF n v e a)
value = do
  token Token.hash
  e <- lexeme valueExpr
  pure $ ValueF e

predicate :: (Source s) => Parse n v e s (ExprF n v e a)
predicate = do
  token Token.question
  e <- lexeme valueExpr
  pure $ PredicateF e

constr :: (Source s) => Parse n v e s (ExprF n v e (ExprL n v e))
constr = do
  n <- lexeme name
  es <- many $ atomParser atom
  pure $ PatternF n es

collection :: (Source s) => Parse n v e s (ExprF n v e (ExprL n v e))
collection = do
  token Token.bracketLeft
  es <- expr `sepBy` token Token.comma
  token Token.bracketRight
  pure $ CollectionF es

not_ :: (Source s) => Parse n v e s (ExprF n v e (ExprL n v e))
not_ = do
  token Token.exclamation
  e <- atomParser atom
  pure $ NotF e

tupleOrParens :: (Source s) => Parse n v e s (ExprF n v e (ExprL n v e))
tupleOrParens = parens $ do
  es <- expr `sepBy` token Token.comma
  pure $ case es of
    [x] -> unwrap x -- parens, discarding location once
    _ -> TupleF es -- tuple

atom :: (Source s) => Parse n v e s (ExprF n v e (ExprL n v e))
atom =
  wildcard
    <|> variable
    <|> not_
    <|> value
    <|> collection
    <|> constr
    <|> predicate
    <|> tupleOrParens
      <?> "atomic pattern"

expr :: (Source s) => Parse n v e s (ExprL n v e)
expr = exprParser primInfixes atom

instance (Source s) => Parsable (Expr n v e) s (ParseMode n v e s) where
  parseNonGreedyWithLocation = runParse (space *> expr)

-- | Parse 'Expr' with locations annotated.
parseExprL ::
  forall m s n v e.
  (Source s, MonadError (Errors s) m) =>
  ParseMode n v e s ->
  s ->
  m (ExprL n v e)
parseExprL = parseWithLocation @(Expr n v e)

-- | Parse 'Expr'.
parseExpr ::
  (Source s, MonadError (Errors s) m) =>
  ParseMode n v e s ->
  s ->
  m (Expr n v e)
parseExpr = parse
