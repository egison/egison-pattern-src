-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Token
-- Description: Tokens in expressions
-- Stability:   experimental
--
-- This module defines a set of tokens for token types.

module Language.Egison.Syntax.Pattern.Parser.Token
  ( IsToken(..)
  )
where

import qualified Data.Char                     as Char
                                                ( isSpace )


class IsToken c where
  isSpace :: c -> Bool
  parenLeft :: c
  parenRight :: c
  underscore :: c
  hash :: c
  question :: c
  exclamation :: c
  and :: c
  vertical :: c
  dollar :: c

instance IsToken Char where
  isSpace     = Char.isSpace
  parenLeft   = '('
  parenRight  = ')'
  underscore  = '_'
  hash        = '#'
  question    = '?'
  exclamation = '!'
  and         = '&'
  vertical    = '|'
  dollar      = '$'
