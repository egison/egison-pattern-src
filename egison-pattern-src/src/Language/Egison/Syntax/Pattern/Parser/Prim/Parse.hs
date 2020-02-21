-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Prim.Parse
-- Description: Parser monad
-- Stability:   experimental
--
-- This module defines a parser monad 'Parse'.

module Language.Egison.Syntax.Pattern.Parser.Prim.Parse
  ( Parse
  , runParse
  )
where

import           Control.Applicative            ( Alternative )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader(..)
                                                , runReaderT
                                                )
import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad                  ( MonadPlus )

import           Text.Megaparsec                ( Parsec
                                                , MonadParsec
                                                )
import qualified Text.Megaparsec               as Parsec
                                                ( Stream
                                                , parse
                                                , eof
                                                , getSourcePos
                                                )

import           Language.Egison.Syntax.Pattern.Parser.Prim.Source
                                                ( Source )
import           Language.Egison.Syntax.Pattern.Parser.Prim.Location
                                                ( Locate(..)
                                                , fromSourcePos
                                                )
import           Language.Egison.Syntax.Pattern.Parser.Prim.ParseMode
                                                ( ParseMode )
import           Language.Egison.Syntax.Pattern.Parser.Prim.Error
                                                ( Errors
                                                , CustomError
                                                , fromParseErrorBundle
                                                )


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
 where
  parsec = runReaderT (unParse file) mode
  file   = parse <* Parsec.eof
