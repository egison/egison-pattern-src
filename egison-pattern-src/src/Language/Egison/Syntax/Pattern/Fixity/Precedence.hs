-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Fixity.Precedence
-- Description: A precedence of infix operators
-- Stability:   experimental
--
-- A precedence of infix operators.
module Language.Egison.Syntax.Pattern.Fixity.Precedence
  ( Precedence (..),
    toInt,
  )
where

import Data.Data
  ( Data,
    Typeable,
  )
import GHC.Generics (Generic)

-- | A precedence of infix operators.
newtype Precedence = Precedence Int
  deriving newtype (Eq, Ord)
  deriving stock (Show, Generic, Data, Typeable)

-- | Obtain an integer representation of 'Precedence'.
toInt :: Precedence -> Int
toInt (Precedence i) = i
