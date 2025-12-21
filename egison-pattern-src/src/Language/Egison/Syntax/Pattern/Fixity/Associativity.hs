-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Fixity.Associativity
-- Description: Associativity of infix operators
-- Stability:   experimental
--
-- An associativity of infix operators.
module Language.Egison.Syntax.Pattern.Fixity.Associativity
  ( Associativity (..),
  )
where

import Data.Data
  ( Data,
    Typeable,
  )
import GHC.Generics (Generic)

-- | Associativity of infix operators.
data Associativity = AssocLeft | AssocRight | AssocNone
  deriving (Show, Eq, Generic, Data, Typeable)
