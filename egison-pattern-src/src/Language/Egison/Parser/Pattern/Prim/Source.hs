{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim.Source
-- Description: Constraint for the source of parser
-- Stability:   experimental
--
-- A constraint and property of the source of parser

module Language.Egison.Parser.Pattern.Prim.Source
  ( Source(..)
  , Token
  , Tokens
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
                                                ( null
                                                , cons
                                                , snoc
                                                )
import qualified Text.Megaparsec               as Parsec
                                                ( Stream(..) )

import           Language.Egison.Parser.Pattern.Token
                                                ( IsToken )


-- | Type of token in the source.
type Token s = Parsec.Token s
-- | Type of tokens in the source.
type Tokens s = Parsec.Tokens s


-- | Constraint for the source of parser.
class (Parsec.Stream s, IsToken (Token s)) => Source s where
  eof :: s -> Bool
  tokens :: s -> Tokens s
  consTokens :: Token s -> Tokens s -> Tokens s
  snocTokens :: Tokens s -> Token s -> Tokens s

instance Source Text where
  eof        = T.null
  tokens     = id
  consTokens = T.cons
  snocTokens = T.snoc

instance Source String where
  eof        = null
  tokens     = id
  consTokens = (:)
  snocTokens xs x = xs ++ [x]
