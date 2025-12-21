-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim.Parse
-- Description: Parser monad
-- Stability:   experimental
--
-- This module defines a parser monad 'Parse'.
module Language.Egison.Parser.Pattern.Prim.Parse
  ( Parse,
    runParse,
  )
where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader
  ( MonadReader (..),
    ReaderT,
    runReaderT,
  )
import Language.Egison.Parser.Pattern.Prim.Error
  ( CustomError,
    Errors,
    fromParseErrorBundle,
  )
import Language.Egison.Parser.Pattern.Prim.Location
  ( Locate (..),
    fromSourcePos,
  )
import Language.Egison.Parser.Pattern.Prim.ParseMode
  ( ParseMode (..),
  )
import Language.Egison.Parser.Pattern.Prim.Source
  ( Source,
  )
import Text.Megaparsec
  ( MonadParsec,
    Parsec,
    PosState (..),
    State (..),
  )
import Text.Megaparsec qualified as Parsec
  ( PosState (..),
    State (..),
    TraversableStream (..),
    defaultTabWidth,
    getSourcePos,
    initialPos,
    runParser',
  )

-- | Parser monad.
newtype Parse n v e s a = Parse {unParse :: ReaderT (ParseMode n v e s) (Parsec (CustomError s) s) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadFail, MonadPlus)
  deriving newtype (MonadReader (ParseMode n v e s))
  deriving newtype (MonadParsec (CustomError s) s)

instance (Parsec.TraversableStream s) => Locate (Parse n v e s) where
  getPosition = fromSourcePos <$> Parsec.getSourcePos

-- | Run 'Parse' monad and produce a parse result.
runParse ::
  (Source s, MonadError (Errors s) m) =>
  Parse n v e s a ->
  ParseMode n v e s ->
  s ->
  m (a, s)
runParse parser mode@ParseMode {filename} content =
  case Parsec.runParser' parsec initState of
    (_, Left bundle) -> throwError $ fromParseErrorBundle bundle
    (Parsec.State {stateInput}, Right e) -> pure (e, stateInput)
  where
    parsec = runReaderT (unParse parser) mode
    initState =
      Parsec.State
        { stateInput = content,
          stateOffset = 0,
          statePosState =
            Parsec.PosState
              { pstateInput = content,
                pstateOffset = 0,
                pstateSourcePos = Parsec.initialPos filename,
                pstateTabWidth = Parsec.defaultTabWidth,
                pstateLinePrefix = ""
              },
          stateParseErrors = []
        }
