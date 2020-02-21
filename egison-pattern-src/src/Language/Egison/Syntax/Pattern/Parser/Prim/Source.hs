module Language.Egison.Syntax.Pattern.Parser.Prim.Source
  ( Source
  , Token
  , Tokens
  )
where

import qualified Text.Megaparsec               as Parsec
                                                ( Stream(..) )

import           Language.Egison.Syntax.Pattern.Parser.Token
                                                ( IsToken )


-- | Constraint for the source of parser.
type Source s = (Parsec.Stream s, IsToken (Token s))
-- | Type of token in the source.
type Token s = Parsec.Token s
-- | Type of tokens in the source.
type Tokens s = Parsec.Tokens s
