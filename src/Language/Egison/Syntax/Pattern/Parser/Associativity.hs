-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Associativity
-- Description: An associativity of infix operators
-- Stability:   experimental
--
-- An associativity of infix operators.
--
-- This module is intended to be imported qualified like below.
--
-- > import           Language.Egison.Syntax.Pattern.Parser.Associativity
-- >                                                 ( Associativity )
-- > import qualified Language.Egison.Syntax.Pattern.Parser.Associativity
-- >                                                as Assoc
-- >                                                 ( Associativity(..) )

module Language.Egison.Syntax.Pattern.Parser.Associativity
  ( Associativity(..)
  )
where


-- | An associativity of infix operators.
data Associativity = Left | Right | None