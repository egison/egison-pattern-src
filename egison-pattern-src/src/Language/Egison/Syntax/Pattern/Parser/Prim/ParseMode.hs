module Language.Egison.Syntax.Pattern.Parser.Prim.ParseMode
  ( ParseMode(..)
  , Fixity(..)
  , ExtParser
  )
where

import           Language.Egison.Syntax.Pattern.Parser.Associativity
                                                ( Associativity )
import           Language.Egison.Syntax.Pattern.Parser.Precedence
                                                ( Precedence )

import           Language.Egison.Syntax.Pattern.Parser.Prim.Source
                                                ( Tokens )


-- | @'ExtParser' s a' is a type for externally provided parser of @a@
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
