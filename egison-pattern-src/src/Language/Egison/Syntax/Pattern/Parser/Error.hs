-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Token
-- Description: Parse errors
-- Stability:   experimental
--
-- This module defines datatypes representing parser errors

module Language.Egison.Syntax.Pattern.Parser.Error
  ( Error(..)
  , ErrorItem(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data
                                                , Typeable
                                                )

import           Language.Egison.Syntax.Pattern.Parser.Location
                                                ( Position )


-- | A token representation in 'Error'.
data ErrorItem tokens
  = Tokens tokens
  | Label String
  | EndOfInput
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A parse error.
data Error tokens
  = UnexpectedToken { position :: Position
                    , expected :: [ErrorItem tokens]
                    , found    :: Maybe (ErrorItem tokens)
                    }
  | ExternalError { position :: Position
                  , input :: tokens
                  , message :: String
                  }
  deriving (Show, Eq, Generic, Data, Typeable)
