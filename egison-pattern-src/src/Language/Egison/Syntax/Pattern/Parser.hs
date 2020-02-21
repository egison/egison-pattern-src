-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser
-- Description: A parser for Egison patterns
-- Stability:   experimental
--
-- A parser for Egison patterns.

module Language.Egison.Syntax.Pattern.Parser
  ( parseExprL
  , parseExpr
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Syntax.Pattern.Parser.Prim
                                               as X
                                                ( Source
                                                , Token
                                                , Tokens
                                                , ExtParser
                                                , ParseMode(..)
                                                , Fixity(..)
                                                , Errors
                                                , Error(..)
                                                , ErrorItem(..)
                                                , Location(..)
                                                , Position(..)
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Expr
                                               as X
                                                ( Precedence(..)
                                                , Associativity(..)
                                                , ExprL
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Token
                                               as X
                                                ( IsToken(..) )

-- main
import           Control.Monad.Except           ( MonadError )
import           Control.Applicative            ( (<|>) )
import           Control.Monad.Combinators      ( many )
import           Control.Comonad.Cofree         ( unwrap )

import           Language.Egison.Syntax.Pattern.Parser.Prim
                                                ( Parse
                                                , runParse
                                                , lexeme
                                                , name
                                                , varName
                                                , valueExpr
                                                , try
                                                , (<?>)
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Combinator
                                                ( token
                                                , parens
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Expr
                                                ( exprParser
                                                , Table(..)
                                                , initTable
                                                , addInfix
                                                , addPrefix
                                                )
import qualified Language.Egison.Syntax.Pattern.Parser.Token
                                               as Token
                                                ( underscore
                                                , hash
                                                , question
                                                , exclamation
                                                , and
                                                , vertical
                                                , dollar
                                                )
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
  [ (Precedence 5, addPrefix (NotF <$ token Token.exclamation) initTable)
  , (Precedence 3, addInfix AssocRight (AndF <$ token Token.and) initTable)
  , (Precedence 2, addInfix AssocRight (OrF <$ token Token.vertical) initTable)
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
parseExprL = runParse expr

-- | A parser for 'Expr'.
parseExpr
  :: (Source s, MonadError (Errors s) m)
  => ParseMode n v e s
  -> FilePath
  -> s
  -> m (Expr n v e)
parseExpr mode path = fmap unAnnotate . parseExprL mode path
