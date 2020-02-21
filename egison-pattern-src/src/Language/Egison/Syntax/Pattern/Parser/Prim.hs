{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , ExtParser
  -- * Parser Monad
  , Parse
  , runParse
  -- * Primitive Parsers
  , extParser
  , space
  , lexeme
  , name
  , varName
  , valueExpr
  -- * Errors
  , Errors
  , Error(..)
  , ErrorItem(..)
  -- * Locations
  , Position(..)
  , Location(..)
  , Locate(..)
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
import           Data.Proxy                     ( Proxy(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader(..)
                                                , runReaderT
                                                )
import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad                  ( MonadPlus
                                                , void
                                                )
import           Control.Applicative            ( Alternative((<|>))
                                                , empty
                                                )
import           Control.Applicative.Combinators
                                                ( between )
import           Text.Megaparsec                ( Parsec )
import qualified Text.Megaparsec               as Parsec
                                                ( parse
                                                , eof
                                                , takeWhile1P
                                                , takeWhileP
                                                , manyTill
                                                , chunk
                                                , chunkToTokens
                                                , tokensToChunk
                                                , Stream(..)
                                                , customFailure
                                                , getSourcePos
                                                , single
                                                , anySingle
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
                                                ( lexeme
                                                , space
                                                )

import           Language.Egison.Syntax.Pattern.Parser.Associativity
                                                ( Associativity )
import           Language.Egison.Syntax.Pattern.Parser.Precedence
                                                ( Precedence )
import qualified Language.Egison.Syntax.Pattern.Parser.Token
                                               as Token
                                                ( isSpace
                                                , parenLeft
                                                , parenRight
                                                , newline
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Prim.Location
                                                ( Position(..)
                                                , Location(..)
                                                , Locate(..)
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Prim.Error
                                                ( Error(..)
                                                , ErrorItem(..)
                                                , Errors
                                                , CustomError(..)
                                                , fromParseErrorBundle
                                                )

import           Language.Egison.Syntax.Pattern.Parser.Prim.Source
                                                ( Source
                                                , Token
                                                , Tokens
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Prim.Location
                                                ( fromSourcePos )


-- | @'ExtParser' s a' is a type for externally provided parser of @a@
type ExtParser s a = Tokens s -> Either String a

-- | Fixity of infix operators.
data Fixity n s =
  Fixity { associativity :: Associativity
         , precedence :: Precedence
         , parser :: ExtParser s n
         }

-- | Parser configuration.
data ParseMode n v e s
  = ParseMode { fixities        :: [Fixity n s]
              , blockComment    :: Maybe (Tokens s, Tokens s)
              , lineComment     :: Maybe (Tokens s)
              , varNameParser   :: ExtParser s v
              , nameParser      :: ExtParser s n
              , valueExprParser :: ExtParser s e
              }


-- | A parser monad.
newtype Parse n v e s a = Parse { unParse :: ReaderT (ParseMode n v e s) (Parsec (CustomError s) s) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus)
  deriving newtype (MonadReader (ParseMode n v e s))
  deriving newtype (MonadParsec (CustomError s) s)

instance Parsec.Stream s => Locate (Parse n v e s) where
  getPosition = fromSourcePos <$> Parsec.getSourcePos


-- | Run 'Parse' monad and produce a parse result.
runParse
  :: (Source s, MonadError (Errors s) m)
  => Parse n v e s a
  -> ParseMode n v e s
  -> FilePath
  -> s
  -> m a
runParse parse mode filename content =
  case Parsec.parse parsec filename content of
    Left  bundle -> throwError $ fromParseErrorBundle bundle
    Right e      -> pure e
  where parsec = runReaderT (unParse $ file parse) mode

file :: Source s => Parse n v e s a -> Parse n v e s a
file = between space Parsec.eof

skipBlockComment :: Source s => Tokens s -> Tokens s -> Parse n v e s ()
skipBlockComment start end = cs *> void (Parsec.manyTill Parsec.anySingle ce)
 where
  cs = Parsec.chunk start
  ce = Parsec.chunk end

skipLineComment :: Source s => Tokens s -> Parse n v e s ()
skipLineComment prefix = Parsec.chunk prefix
  *> void (Parsec.takeWhileP (Just "chars") (/= Token.newline))

-- | Skip one or more spaces.
space :: Source s => Parse n v e s ()
space = do
  ParseMode { blockComment, lineComment } <- ask
  let block = emptyOr (uncurry skipBlockComment) blockComment
      line  = emptyOr skipLineComment lineComment
  L.space space1 line block
 where
  space1  = void $ Parsec.takeWhile1P (Just "whitespace") Token.isSpace
  emptyOr = maybe empty

-- | Parse a lexical chunk.
takeChunk :: forall n v e s . Source s => Parse n v e s (Tokens s)
takeChunk = withParens <|> withoutParens
 where
  withParens = do
    left <- Parsec.single Token.parenLeft
    ck   <- Parsec.takeWhileP (Just "lexical chunk (in parens)")
                              endOfChunkInParens
    right <- Parsec.single Token.parenRight
    -- TODO: better solution?
    let tk = left : Parsec.chunkToTokens (Proxy @s) ck ++ [right]
    pure $ Parsec.tokensToChunk (Proxy @s) tk
  withoutParens = Parsec.takeWhileP (Just "lexical chunk") endOfChunk
  endOfChunkInParens x = x /= Token.parenRight
  endOfChunk x = not (Token.isSpace x) && x /= Token.parenRight

-- | Apply an external parser.
extParser :: Source s => ExtParser s a -> Parse n v e s a
extParser p = try $ do
  lchunk <- takeChunk
  case p lchunk of
    Left  err -> Parsec.customFailure (ExtParserError lchunk err)
    Right x   -> pure x

-- | Make a lexical token.
-- @lexeme p@ first applies parser @p@ then 'space' parser.
lexeme :: Source s => Parse n v e s a -> Parse n v e s a
lexeme = L.lexeme space

-- | Parser for @n@ in @Parse n v e s@ monad.
name :: Source s => Parse n v e s n
name = do
  ParseMode { nameParser } <- ask
  extParser nameParser

-- | Parser for @v@ in @Parse n v e s@ monad.
varName :: Source s => Parse n v e s v
varName = do
  ParseMode { varNameParser } <- ask
  extParser varNameParser

-- | Parser for @e@ in @Parse n v e s@ monad.
valueExpr :: Source s => Parse n v e s e
valueExpr = do
  ParseMode { valueExprParser } <- ask
  extParser valueExprParser
