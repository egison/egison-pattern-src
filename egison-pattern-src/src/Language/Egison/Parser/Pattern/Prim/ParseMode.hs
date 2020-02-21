-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim.ParseMode
-- Description: Parser configuration
-- Stability:   experimental
--
-- A parser configuration type, that contains a set of external parsers

module Language.Egison.Parser.Pattern.Prim.ParseMode
  ( ParseMode(..)
  , Fixity(..)
  , ExtParser
  )
where

import           Language.Egison.Parser.Pattern.Associativity
                                                ( Associativity )
import           Language.Egison.Parser.Pattern.Precedence
                                                ( Precedence )

import           Language.Egison.Parser.Pattern.Prim.Source
                                                ( Tokens )


-- | @'ExtParser' s a@ is a type for externally provided parser of @a@.
type ExtParser s a = Tokens s -> Either String a

-- | Fixity of infix operators.
data Fixity n s =
  Fixity { associativity :: Associativity
         , precedence :: Precedence
         , parser :: ExtParser s n
         }

-- | Parser configuration.
data ParseMode n v e s
  = ParseMode { fixities        :: [Fixity n s]
              , blockComment    :: Maybe (Tokens s, Tokens s)
              , lineComment     :: Maybe (Tokens s)
              , varNameParser   :: ExtParser s v
              , nameParser      :: ExtParser s n
              , valueExprParser :: ExtParser s e
              }
