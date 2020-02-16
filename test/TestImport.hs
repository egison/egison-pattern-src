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
import           Control.Applicative            ( some )
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
                                                , Errors
                                                , parseExpr
                                                )
import qualified Language.Egison.Syntax.Pattern.Parser
                                               as Assoc
                                                ( Associativity(..) )


newtype Name = Name String
  deriving (Show, Eq)

newtype ValueExpr = ValueExprInt Int
  deriving (Show, Eq)

unParsec :: Parsec Void String a -> (String -> Either String a)
unParsec p input = case Parsec.parse p "test" input of
  Left  _ -> Left "error!"
  Right x -> Right x

testFixities :: [Fixity Name String]
testFixities =
  [ Fixity Assoc.Right (Precedence 5) (unParsec (Name "++" <$ pp))
  , Fixity Assoc.Right (Precedence 5) (unParsec (Name ":" <$ col))
  , Fixity Assoc.Left  (Precedence 4) (unParsec (Name "|>" <$ rear))
  , Fixity Assoc.Right (Precedence 4) (unParsec (Name "<|" <$ front))
  ]
 where
  pp    = Parsec.chunk "++"
  col   = Parsec.single ':'
  rear  = Parsec.chunk "|>"
  front = Parsec.chunk "<|"

testParseName :: Parsec Void String Name
testParseName = Name <$> some Parsec.letterChar

testParseValueExpr :: Parsec Void String ValueExpr
testParseValueExpr = ValueExprInt <$> Parsec.decimal

testMode :: ParseMode Name ValueExpr String
testMode = ParseMode { filename        = "test"
                     , fixities        = testFixities
                     , blockComment    = Just ("{-", "-}")
                     , lineComment     = Just "--"
                     , nameParser      = unParsec testParseName
                     , valueExprParser = unParsec testParseValueExpr
                     }

testParseExpr
  :: MonadError (Errors String) m => String -> m (Expr Name ValueExpr)
testParseExpr = parseExpr testMode
