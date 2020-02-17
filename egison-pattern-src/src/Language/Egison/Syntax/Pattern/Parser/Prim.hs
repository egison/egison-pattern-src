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
                                                ( toList
                                                , size
                                                , elemAt
                                                )
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
                                                , ParseErrorBundle(..)
                                                , ParseError(..)
                                                , ErrorFancy(..)
                                                , ErrorItem(..)
                                                , Stream(..)
                                                , Token
                                                , Tokens
                                                , SourcePos(..)
                                                , customFailure
                                                , errorOffset
                                                , attachSourcePos
                                                , getSourcePos
                                                , unPos
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
import           Language.Egison.Syntax.Pattern.Parser.Token
                                                ( IsToken )
import qualified Language.Egison.Syntax.Pattern.Parser.Token
                                               as Token
                                                ( isSpace
                                                , parenLeft
                                                , parenRight
                                                , newline
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
              , blockComment    :: Maybe (Tokens s, Tokens s)
              , lineComment     :: Maybe (Tokens s)
              , nameParser      :: ExtParser s n
              , valueExprParser :: ExtParser s e
              }

-- | An internal error type to use as a custom error in 'Parsec'
data CustomError s = ExtParserError { input :: Tokens s
                                    , message :: String
                                    }

deriving instance Eq (Tokens s) => Eq (CustomError s)
deriving instance Ord (Tokens s) => Ord (CustomError s)

-- | A parser monad.
newtype Parse n e s a = Parse { unParse :: ReaderT (ParseMode n e s) (Parsec (CustomError s) s) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus)
  deriving newtype (MonadReader (ParseMode n e s))
  deriving newtype (MonadParsec (CustomError s) s)

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
  case Parsec.parse parsec filename content of
    Left  bundle -> throwError $ makeErrors bundle
    Right e      -> pure e
  where parsec = runReaderT (unParse $ file parse) mode

file :: Source s => Parse n e s a -> Parse n e s a
file = between space Parsec.eof

skipBlockComment :: Source s => Tokens s -> Tokens s -> Parse n e s ()
skipBlockComment start end = cs *> void (Parsec.manyTill Parsec.anySingle ce)
 where
  cs = Parsec.chunk start
  ce = Parsec.chunk end

skipLineComment :: Source s => Tokens s -> Parse n e s ()
skipLineComment prefix = Parsec.chunk prefix
  *> void (Parsec.takeWhileP (Just "chars") (/= Token.newline))

-- | Skip one or more spaces.
space :: Source s => Parse n e s ()
space = do
  ParseMode { blockComment, lineComment } <- ask
  let block = emptyOr (uncurry skipBlockComment) blockComment
      line  = emptyOr skipLineComment lineComment
  L.space space1 line block
 where
  space1  = void $ Parsec.takeWhile1P (Just "whitespace") Token.isSpace
  emptyOr = maybe empty

-- | Parse a lexical chunk.
takeChunk :: forall n e s . Source s => Parse n e s (Tokens s)
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
extParser :: Source s => ExtParser s a -> Parse n e s a
extParser p = try $ do
  lchunk <- takeChunk
  case p lchunk of
    Left  err -> Parsec.customFailure (ExtParserError lchunk err)
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
  Position { line, column }
 where
  line   = Parsec.unPos sourceLine
  column = Parsec.unPos sourceColumn

makeErrorItem
  :: forall s
   . Parsec.Stream s
  => Parsec.ErrorItem (Token s)
  -> ErrorItem (Tokens s)
makeErrorItem (Parsec.Tokens ts) =
  Tokens . Parsec.tokensToChunk (Proxy @s) $ NonEmpty.toList ts
makeErrorItem (Parsec.Label cs) = Label $ NonEmpty.toList cs
makeErrorItem Parsec.EndOfInput = EndOfInput

makeFancyError
  :: Parsec.SourcePos -> Parsec.ErrorFancy (CustomError s) -> Error (Tokens s)
makeFancyError pos (Parsec.ErrorCustom err) = extError
 where
  position                          = makePosition pos
  ExtParserError { input, message } = err
  extError                          = ExternalError { position, input, message }
makeFancyError _ _ = error "unreachable: unused fancy error"

makeError
  :: forall s
   . Parsec.Stream s
  => (Parsec.ParseError s (CustomError s), Parsec.SourcePos)
  -> Error (Tokens s)
makeError (Parsec.FancyError _ es, pos) | Set.size es == 1 =
  makeFancyError pos $ Set.elemAt 0 es
makeError (Parsec.TrivialError _ mfound expectedSet, pos) = UnexpectedToken
  { position
  , expected
  , found
  }
 where
  found    = fmap (makeErrorItem @s) mfound
  expected = map (makeErrorItem @s) $ Set.toList expectedSet
  position = makePosition pos
makeError _ = error "unreachable: unused error"

makeErrors
  :: Parsec.Stream s => Parsec.ParseErrorBundle s (CustomError s) -> Errors s
makeErrors Parsec.ParseErrorBundle { Parsec.bundleErrors = errors, Parsec.bundlePosState = posState }
  = fmap makeError errorsWithPos
 where
  (errorsWithPos, _) =
    Parsec.attachSourcePos Parsec.errorOffset errors posState
