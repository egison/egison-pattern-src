{-# LANGUAGE AllowAmbiguousTypes #-}

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
  , valueExpr
  -- * Error Type
  , Errors
  , Error(..)
  , ErrorItem(..)
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
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NonEmpty
                                                ( toList )
import qualified Data.Set                      as Set
                                                ( toList )
import           Data.Void                      ( Void )
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
import           Text.Megaparsec                ( Parsec )
import qualified Text.Megaparsec               as Parsec
                                                ( parse
                                                , eof
                                                , takeWhile1P
                                                , takeWhileP
                                                , ParseErrorBundle(..)
                                                , ParseError(..)
                                                , ErrorItem(..)
                                                , Stream(..)
                                                , Token
                                                , Tokens
                                                , SourcePos(..)
                                                , errorOffset
                                                , attachSourcePos
                                                , getSourcePos
                                                , unPos
                                                , single
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L
                                                ( lexeme
                                                , space
                                                )

import           Language.Egison.Syntax.Pattern.Parser.Associativity
                                                ( Associativity )
import           Language.Egison.Syntax.Pattern.Parser.Precedence
                                                ( Precedence )
import           Language.Egison.Syntax.Pattern.Parser.Token
                                                ( IsToken )
import qualified Language.Egison.Syntax.Pattern.Parser.Token
                                               as Token
                                                ( isSpace
                                                , parenLeft
                                                , parenRight
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Location
                                                ( Position(..)
                                                , Locate(..)
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Error
                                                ( Error(..)
                                                , ErrorItem(..)
                                                )


-- | Constraint for parser stream.
type Source s = (Parsec.Stream s, IsToken (Parsec.Token s))
type Token s = Parsec.Token s
type Tokens s = Parsec.Tokens s

-- | @'ExtParser' s a' is a type for externally provided parser of @a@
type ExtParser s a = Tokens s -> Either String a

-- | Fixity of infix operators.
data Fixity n s =
  Fixity { associativity :: Associativity
         , precedence :: Precedence
         , parser :: ExtParser s n
         }

-- | Parser configuration.
data ParseMode n e s
  = ParseMode { filename        :: FilePath
              , fixities        :: [Fixity n s]
              , nameParser      :: ExtParser s n
              , valueExprParser :: ExtParser s e
              }

-- | A parser monad.
newtype Parse n e s a = Parse { unParse :: ReaderT (ParseMode n e s) (Parsec Void s) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus)
  deriving newtype (MonadReader (ParseMode n e s))
  deriving newtype (MonadParsec Void s)

instance Parsec.Stream s => Locate (Parse n e s) where
  getPosition = makePosition <$> Parsec.getSourcePos


-- | Run 'Parse' monad and produce a parse result.
runParse
  :: (Source s, MonadError (Errors s) m)
  => Parse n e s a
  -> ParseMode n e s
  -> s
  -> m a
runParse parse mode@ParseMode { filename } content =
  case Parsec.parse parser filename content of
    Left  bundle -> throwError $ makeErrors bundle
    Right e      -> pure e
  where parser = runReaderT (unParse parse) mode <* Parsec.eof

-- | Skip one or more spaces.
space :: Source s => Parse n e s ()
space = L.space space1 empty empty
  where space1 = void $ Parsec.takeWhile1P (Just "whitespace") Token.isSpace

-- | Parse a lexical chunk.
takeChunk :: Source s => Parse n e s (Tokens s)
takeChunk = withParens <|> withoutParens
 where
  withParens = do
    void $ Parsec.single Token.parenLeft
    Parsec.takeWhileP (Just "lexical chunk (in parens)") endOfChunkInParens
  withoutParens = Parsec.takeWhileP (Just "lexical chunk") endOfChunk
  endOfChunkInParens x = x /= Token.parenRight
  endOfChunk x = not (Token.isSpace x) && x /= Token.parenRight

-- | Apply an external parser.
extParser :: Source s => ExtParser s a -> Parse n e s a
extParser p = try $ do
  lchunk <- takeChunk
  case p lchunk of
    Left  err -> fail err
    Right x   -> pure x

-- | Make a lexical token.
-- @lexeme p@ first applies parser @p@ then 'space' parser.
lexeme :: Source s => Parse n e s a -> Parse n e s a
lexeme = L.lexeme space

-- | Parser for @n@ in @Parse n e s@ monad.
name :: Source s => Parse n e s n
name = do
  ParseMode { nameParser } <- ask
  extParser nameParser

-- | Parser for @e@ in @Parse n e s@ monad.
valueExpr :: Source s => Parse n e s e
valueExpr = do
  ParseMode { valueExprParser } <- ask
  extParser valueExprParser


-- | A type synonym for an error list.
type Errors s = NonEmpty (Error (Tokens s))

makePosition :: Parsec.SourcePos -> Position
makePosition Parsec.SourcePos { Parsec.sourceLine, Parsec.sourceColumn } =
  Position { line   = Parsec.unPos sourceLine
           , column = Parsec.unPos sourceColumn
           }

makeErrorItem
  :: forall s
   . Parsec.Stream s
  => Parsec.ErrorItem (Token s)
  -> ErrorItem (Tokens s)
makeErrorItem (Parsec.Tokens ts) =
  Tokens . Parsec.tokensToChunk (Proxy @s) $ NonEmpty.toList ts
makeErrorItem (Parsec.Label cs) = Label $ NonEmpty.toList cs
makeErrorItem Parsec.EndOfInput = EndOfInput

makeError
  :: forall s e
   . Parsec.Stream s
  => (Parsec.ParseError s e, Parsec.SourcePos)
  -> Error (Tokens s)
makeError (Parsec.FancyError _ _, _) = error "we don't use fancy errors"
makeError (Parsec.TrivialError _ mfound expectedSet, pos) = UnexpectedToken
  { position
  , expected
  , found
  }
 where
  found    = fmap (makeErrorItem @s) mfound
  expected = map (makeErrorItem @s) $ Set.toList expectedSet
  position = makePosition pos

makeErrors :: Parsec.Stream s => Parsec.ParseErrorBundle s e -> Errors s
makeErrors Parsec.ParseErrorBundle { Parsec.bundleErrors = errors, Parsec.bundlePosState = posState }
  = fmap makeError errorsWithPos
 where
  (errorsWithPos, _) =
    Parsec.attachSourcePos Parsec.errorOffset errors posState
