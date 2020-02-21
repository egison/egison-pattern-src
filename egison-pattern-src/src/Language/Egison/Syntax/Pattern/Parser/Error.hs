{-# LANGUAGE UndecidableInstances #-}

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

import           Language.Egison.Syntax.Pattern.Parser.Prim.Source
                                                ( Tokens )
import           Language.Egison.Syntax.Pattern.Parser.Location
                                                ( Position )


-- | A token representation in 'Error'.
data ErrorItem s
  = Tokens (Tokens s)
  | Label String
  | EndOfInput

deriving instance Show (Tokens s) => Show (ErrorItem s)
deriving instance Eq (Tokens s) => Eq (ErrorItem s)


-- | A parse error.
data Error s
  = UnexpectedToken { position :: Position
                    , expected :: [ErrorItem s]
                    , found    :: Maybe (ErrorItem s)
                    }
  | ExternalError { position :: Position
                  , input :: Tokens s
                  , message :: String
                  }

deriving instance Show (Tokens s) => Show (Error s)
deriving instance Eq (Tokens s) => Eq (Error s)
