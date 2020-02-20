-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Associativity
-- Description: An associativity of infix operators
-- Stability:   experimental
--
-- An associativity of infix operators.

module Language.Egison.Syntax.Pattern.Parser.Associativity
  ( Associativity(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data
                                                , Typeable
                                                )


-- | An associativity of infix operators.
data Associativity = AssocLeft | AssocRight | AssocNone
  deriving (Show, Eq, Generic, Data, Typeable)
