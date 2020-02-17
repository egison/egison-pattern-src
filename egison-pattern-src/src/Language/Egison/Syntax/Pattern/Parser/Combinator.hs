-- |
--
-- Module:      Language.Egison.Syntax.Pattern.Parser.Combinator
-- Description: Useful combinators to build lexeme parsers
-- Stability:   experimental
--
-- A helper module to build lexeme parsers.
--
-- Note that functions exported from this module produce lexeme parsers (i.e. skip spaces after that),
-- while parsers from 'Language.Egison.Pattern.Parser.Prim' do not.

module Language.Egison.Syntax.Pattern.Parser.Combinator
  ( token
  , symbol
  , parens
  )
where

import           Control.Monad.Combinators      ( between )
import           Data.Functor                   ( void )

import           Language.Egison.Syntax.Pattern.Parser.Prim
                                                ( Parse
                                                , Source
                                                , Token
                                                , Tokens
                                                , lexeme
                                                , single
                                                , chunk
                                                )
import qualified Language.Egison.Syntax.Pattern.Parser.Token
                                               as Token
                                                ( parenLeft
                                                , parenRight
                                                )


-- | Parser for a single token.
token :: Source s => Token s -> Parse n e s ()
token = void . lexeme . single

-- | Parser for a chunk of tokens.
symbol :: Source s => Tokens s -> Parse n e s ()
symbol = void . lexeme . chunk

-- | Wrap a parser with parentheses.
parens :: Source s => Parse n e s a -> Parse n e s a
parens = between (token Token.parenLeft) (token Token.parenRight)
