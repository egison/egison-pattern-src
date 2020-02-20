module TestImport
  ( testParseExpr
  , Name(..)
  , ValueExpr(..)
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Syntax.Pattern.Expr
                                               as X

-- main
import           Data.Void                      ( Void )
import           Control.Applicative            ( (<|>)
                                                , some
                                                )
import           Control.Monad.Except           ( MonadError )

import           Text.Megaparsec                ( Parsec )
import qualified Text.Megaparsec               as Parsec
                                                ( chunk
                                                , single
                                                , parse
                                                )
import qualified Text.Megaparsec.Char          as Parsec
                                                ( letterChar )
import qualified Text.Megaparsec.Char.Lexer    as Parsec
                                                ( decimal )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )
import           Language.Egison.Syntax.Pattern.Parser
                                                ( ParseMode(..)
                                                , Fixity(..)
                                                , Precedence(..)
                                                , Associativity(..)
                                                , Errors
                                                , parseExpr
                                                )


newtype Name = Name String
  deriving (Show, Eq)

newtype ValueExpr = ValueExprInt Int
  deriving newtype Num
  deriving stock (Show, Eq)

unParsec :: Parsec Void String a -> (String -> Either String a)
unParsec p input = case Parsec.parse p "test" input of
  Left  e -> Left (show e)
  Right x -> Right x

testFixities :: [Fixity Name String]
testFixities =
  [ Fixity AssocRight (Precedence 5) (unParsec pp)
  , Fixity AssocRight (Precedence 5) (unParsec col)
  , Fixity AssocLeft  (Precedence 4) (unParsec rear)
  , Fixity AssocRight (Precedence 4) (unParsec front)
  ]
 where
  pp    = Name <$> Parsec.chunk "++"
  col   = Name <$> Parsec.chunk ":"
  rear  = Name <$> Parsec.chunk "|>"
  front = Name <$> Parsec.chunk "<|"

testParseName :: Parsec Void String Name
testParseName = withParens <|> name
 where
  name       = Name <$> some Parsec.letterChar
  ops        = Parsec.chunk "++"
  withParens = do
    op <- Parsec.single '(' *> ops <* Parsec.single ')'
    pure $ Name op

testParseValueExpr :: Parsec Void String ValueExpr
testParseValueExpr = withParens <|> dec
 where
  dec        = ValueExprInt <$> Parsec.decimal
  withParens = do
    d <- Parsec.chunk "(-" *> dec <* Parsec.single ')'
    pure $ negate d

testMode :: ParseMode Name Name ValueExpr String
testMode = ParseMode { fixities        = testFixities
                     , blockComment    = Just ("{-", "-}")
                     , lineComment     = Just "--"
                     , varNameParser   = unParsec testParseName
                     , nameParser      = unParsec testParseName
                     , valueExprParser = unParsec testParseValueExpr
                     }

testParseExpr
  :: MonadError (Errors String) m => String -> m (Expr Name Name ValueExpr)
testParseExpr = parseExpr testMode "test"
