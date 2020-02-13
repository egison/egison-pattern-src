-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Precedence
-- Description: A precedence of infix operators
-- Stability:   experimental
--
-- A precedence of infix operators.

module Language.Egison.Syntax.Pattern.Parser.Precedence
  ( Precedence(..)
  , toInt
  )
where


-- | A precedence of infix operators.
newtype Precedence = Precedence Int
  deriving newtype (Eq, Ord)

-- | Obtain an integer representation of 'Precedence'.
toInt :: Precedence -> Int
toInt (Precedence i) = i
