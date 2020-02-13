-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Precedence
-- Description: A parser monad and primitive parsers
-- Stability:   experimental
--
-- A parser monad and primitive parsers.
--
-- Note that all dependencies on parser library are in this module.

module Language.Egison.Syntax.Pattern.Parser.Prim
  (
  -- * Parser Configuration
    Fixity(..)
  , ParseMode(..)
  -- * Parser Monad
  , Parse
  , runParse
  , liftP
  -- * Primitive Parsers
  , space
  , lexeme
  , name
  , valueExpr
  -- * Source Stream Class
  , Source
  , Token
  , Tokens
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Text.Megaparsec               as X
                                                ( MonadParsec(..)
                                                , (<?>)
                                                , single
                                                , chunk
                                                )

-- main
import           Data.Void                      ( Void )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader(..)
                                                , runReaderT
                                                )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad                  ( MonadPlus
                                                , void
                                                )
import           Control.Applicative            ( Alternative
                                                , empty
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
                                                ( lexeme
                                                , space
                                                )

import           Text.Megaparsec                ( Parsec )
import qualified Text.Megaparsec               as Parsec
                                                ( parse
                                                , takeWhile1P
                                                , eof
                                                , errorBundlePretty
                                                , Stream
                                                , Token
                                                , Tokens
                                                )

import           Language.Egison.Syntax.Pattern.Parser.Associativity
                                                ( Associativity )
import           Language.Egison.Syntax.Pattern.Parser.Precedence
                                                ( Precedence )
import           Language.Egison.Syntax.Pattern.Parser.Token
                                                ( IsToken )
import qualified Language.Egison.Syntax.Pattern.Parser.Token
                                               as Token
                                                ( isSpace )


-- | Constraint for parser stream.
type Source s = (Parsec.Stream s, IsToken (Parsec.Token s))
type Token s = Parsec.Token s
type Tokens s = Parsec.Tokens s

-- | Fixity of infix operators.
data Fixity n s =
  Fixity { associativity :: Associativity
         , precedence :: Precedence
         , parser :: Parsec Void s n
         }

-- | Parser configuration.
data ParseMode n e s
  = ParseMode { filename  :: FilePath
              , fixities  :: [Fixity n s]
              , parseName :: Parsec Void s n
              , parseValueExpr :: Parsec Void s e
              }

-- | A parser monad.
newtype Parse n e s a = Parse { unParse :: ReaderT (ParseMode n e s) (Parsec Void s) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus)
  deriving newtype (MonadReader (ParseMode n e s))
  deriving newtype (MonadParsec Void s)

-- | Lift 'Parsec' monad to 'Parse'.
liftP :: Source s => Parsec Void s a -> Parse n e s a
liftP p = Parse $ lift p

-- | Run 'Parse' monad and produce a parse result.
runParse
  :: (Source s, MonadFail m) => Parse n e s a -> ParseMode n e s -> s -> m a
runParse parse mode@ParseMode { filename } content =
  case Parsec.parse parser filename content of
    Left  bundle -> fail $ Parsec.errorBundlePretty bundle
    Right e      -> pure e
  where parser = runReaderT (unParse parse) mode <* Parsec.eof

-- | Skip one or more spaces.
space :: Source s => Parse n e s ()
space = L.space space1 empty empty
  where space1 = void $ Parsec.takeWhile1P (Just "whitespace") Token.isSpace

-- | Make an lexical token.
-- @lexeme p@ first applies parser @p@ then 'space' parser.
lexeme :: Source s => Parse n e s a -> Parse n e s a
lexeme = L.lexeme space

-- | Parser for @n@ in @Parse n e s@ monad.
name :: Source s => Parse n e s n
name = do
  ParseMode { parseName } <- ask
  n                       <- liftP parseName
  pure n

-- | Parser for @e@ in @Parse n e s@ monad.
valueExpr :: Source s => Parse n e s e
valueExpr = do
  ParseMode { parseValueExpr } <- ask
  e                            <- liftP parseValueExpr
  pure e
