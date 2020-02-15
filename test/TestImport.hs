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
import           Control.Applicative            ( some
                                                , empty
                                                )
import           Control.Monad.Fail             ( MonadFail )

import           Text.Megaparsec                ( Parsec )
import qualified Text.Megaparsec               as Parsec
                                                ( chunk
                                                , single
                                                )
import qualified Text.Megaparsec.Char          as Parsec
                                                ( letterChar
                                                , space1
                                                )
import qualified Text.Megaparsec.Char.Lexer    as Parsec
                                                ( decimal
                                                , space
                                                )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )
import           Language.Egison.Syntax.Pattern.Parser
                                                ( ParseMode(..)
                                                , Fixity(..)
                                                , Precedence(..)
                                                , parseExpr
                                                )
import qualified Language.Egison.Syntax.Pattern.Parser
                                               as Assoc
                                                ( Associativity(..) )


newtype Name = Name String
  deriving (Show, Eq)

newtype ValueExpr = ValueExprInt Int
  deriving (Show, Eq)

testFixities :: [Fixity Name String]
testFixities =
  [ Fixity Assoc.Right (Precedence 5) (Name "++" <$ pp)
  , Fixity Assoc.Right (Precedence 5) (Name ":" <$ col)
  , Fixity Assoc.Left  (Precedence 4) (Name "|>" <$ rear)
  , Fixity Assoc.Right (Precedence 4) (Name "<|" <$ front)
  ]
 where
  pp    = Parsec.chunk "++"
  col   = Parsec.single ':'
  rear  = Parsec.chunk "|>"
  front = Parsec.chunk "<|"

testParseSpace :: Parsec Void String ()
testParseSpace = Parsec.space Parsec.space1 empty empty

testParseName :: Parsec Void String Name
testParseName = Name <$> some Parsec.letterChar

testParseValueExpr :: Parsec Void String ValueExpr
testParseValueExpr = ValueExprInt <$> Parsec.decimal

testMode :: ParseMode Name ValueExpr String
testMode = ParseMode { filename       = "test"
                     , fixities       = testFixities
                     , parseSpace     = testParseSpace
                     , parseName      = testParseName
                     , parseValueExpr = testParseValueExpr
                     }

testParseExpr :: MonadFail m => String -> m (Expr Name ValueExpr)
testParseExpr = parseExpr testMode
