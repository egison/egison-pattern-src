-- |
--
-- Module:      Language.Egison.Parser.Pattern
-- Description: A parser for Egison patterns
-- Stability:   experimental
--
-- A parser for Egison patterns.

module Language.Egison.Parser.Pattern
  ( parseExprL
  , parseExpr
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Parser.Pattern.Prim
                                               as X
                                                ( Source
                                                , Token
                                                , Tokens
                                                , ExtParser
                                                , ParseMode(..)
                                                , ParseFixity(..)
                                                , Errors
                                                , Error(..)
                                                , ErrorItem(..)
                                                , Location(..)
                                                , Position(..)
                                                )
import           Language.Egison.Parser.Pattern.Expr
                                               as X
                                                ( Precedence(..)
                                                , Associativity(..)
                                                , Fixity(..)
                                                , ExprL
                                                )
import           Language.Egison.Parser.Pattern.Token
                                               as X
                                                ( IsToken(..) )

-- main
import           Control.Monad.Except           ( MonadError )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.Combinators      ( many )
import           Control.Comonad.Cofree         ( unwrap )

import           Language.Egison.Parser.Pattern.Prim
                                                ( Parse
                                                , runParse
                                                , lexeme
                                                , space
                                                , name
                                                , varName
                                                , valueExpr
                                                , try
                                                , (<?>)
                                                )
import           Language.Egison.Parser.Pattern.Combinator
                                                ( token
                                                , parens
                                                )
import           Language.Egison.Parser.Pattern.Expr
                                                ( exprParser
                                                , Table(..)
                                                , initTable
                                                , addInfix
                                                , addPrefix
                                                )
import qualified Language.Egison.Parser.Pattern.Token
                                               as Token
                                                ( underscore
                                                , hash
                                                , question
                                                , exclamation
                                                , and
                                                , vertical
                                                , dollar
                                                )
import qualified Language.Egison.Syntax.Pattern.Fixity.Primitive
                                               as PrimOp
import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr )
import           Language.Egison.Syntax.Pattern.Base
                                                ( ExprF(..) )
import           Language.Egison.Syntax.Pattern.Combinator
                                                ( unAnnotate )


primInfixes
  :: Source s
  => [(Precedence, Table (Parse n v e s) (ExprF n v e) (ExprL n v e))]
primInfixes =
  [ ( PrimOp.notPrecedence
    , addPrefix (NotF <$ token Token.exclamation) initTable
    )
  , ( PrimOp.andPrecedence
    , addInfix PrimOp.andAssociativity (AndF <$ token Token.and) initTable
    )
  , ( PrimOp.orPrecedence
    , addInfix PrimOp.orAssociativity (OrF <$ token Token.vertical) initTable
    )
  ]

wildcard :: Source s => Parse n v e s (ExprF n v e a)
wildcard = WildcardF <$ token Token.underscore

variable :: Source s => Parse n v e s (ExprF n v e a)
variable = do
  token Token.dollar
  v <- lexeme varName
  pure $ VariableF v

value :: Source s => Parse n v e s (ExprF n v e a)
value = do
  token Token.hash
  e <- lexeme valueExpr
  pure $ ValueF e

predicate :: Source s => Parse n v e s (ExprF n v e a)
predicate = do
  token Token.question
  e <- lexeme valueExpr
  pure $ PredicateF e

constr :: Source s => Parse n v e s (ExprF n v e (ExprL n v e))
constr = withArgs <|> withoutArgs
 where
  withArgs = parens $ do
    n  <- lexeme name
    es <- many expr
    pure $ PatternF n es
  withoutArgs = do
    n <- lexeme name
    pure $ PatternF n []

atom :: Source s => Parse n v e s (ExprF n v e (ExprL n v e))
atom =
  try (unwrap <$> parens expr) -- discarding location once
    <|> wildcard
    <|> variable
    <|> value
    <|> constr
    <|> predicate
    <?> "atomic pattern"

expr :: Source s => Parse n v e s (ExprL n v e)
expr = exprParser primInfixes atom

-- | A parser for 'Expr' with locations annotated.
parseExprL
  :: (Source s, MonadError (Errors s) m)
  => ParseMode n v e s
  -> FilePath
  -> s
  -> m (ExprL n v e)
parseExprL = runParse (space *> expr)

-- | A parser for 'Expr'.
parseExpr
  :: (Source s, MonadError (Errors s) m)
  => ParseMode n v e s
  -> FilePath
  -> s
  -> m (Expr n v e)
parseExpr mode path = fmap unAnnotate . parseExprL mode path
