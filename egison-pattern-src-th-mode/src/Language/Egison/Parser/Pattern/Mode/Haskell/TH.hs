-- |
--
-- Module:      Language.Egison.Parser.Pattern.Mode.Haskell
-- Description: Parser for Egison pattern expressions to use with Template Haskell
-- Stability:   experimental
--
-- A parser for Egison pattern expressions to use with Template Haskell.
module Language.Egison.Parser.Pattern.Mode.Haskell.TH
  ( -- * Parsers
    Expr,
    ExprL,
    ParseMode (..),
    parseExpr,
    parseExprL,

    -- * Converting @haskell-src-exts@'s entities
    makeParseMode,
    makeFixity,
    makeParseFixity,
  )
where

import Control.Monad.Except (MonadError)
import Language.Egison.Parser.Pattern
  ( Errors,
    Parsable (..),
    Precedence (..),
  )
import Language.Egison.Parser.Pattern qualified as Egison
  ( Associativity (..),
    ExprL,
    Fixity (..),
    ParseFixity (..),
    ParseMode (..),
  )
import Language.Egison.Syntax.Pattern qualified as Egison
  ( Expr,
  )
import Language.Haskell.Exts.Fixity qualified as Haskell
  ( Fixity (..),
  )
import Language.Haskell.Exts.Parser qualified as Haskell
  ( ParseMode (..),
    ParseResult (..),
    parseExpWithMode,
  )
import Language.Haskell.Exts.Syntax qualified as Haskell
  ( Assoc (..),
  )
import Language.Haskell.Meta.Syntax.Translate qualified as TH
  ( toExp,
    toName,
  )
import Language.Haskell.TH.PprLib qualified as TH
  ( pprName',
    to_HPJ_Doc,
  )
import Language.Haskell.TH.Syntax qualified as TH
  ( Exp (..),
    Name,
    NameIs (..),
  )
import Text.PrettyPrint qualified as PP
  ( render,
  )

-- | Type synonym of 'Egison.Expr' to be used with Template Haskell.
type Expr = Egison.Expr TH.Name TH.Name TH.Exp

-- | Type synonym of 'Egison.ExprL' to be used with Template Haskell.
type ExprL = Egison.ExprL TH.Name TH.Name TH.Exp

-- | Parser configuration in @egison-pattern-src-th-mode@.
data ParseMode
  = ParseMode
  { -- | 'Haskell.ParseMode' from @haskell-src-exts@ for our parsers to base on.
    haskellMode :: Haskell.ParseMode,
    -- | List of fixities to parse infix pattern operators.
    -- If @fixities = Just xs@, @xs@ overrides fixities obtained from 'haskellMode'.
    -- Otherwise, our parsers use fixities from 'haskellMode'.
    fixities :: Maybe [Egison.ParseFixity TH.Name String]
  }

resultToEither :: Haskell.ParseResult a -> Either String a
resultToEither (Haskell.ParseOk a) = Right a
resultToEither (Haskell.ParseFailed _ e) = Left e

parseNameWithMode :: Haskell.ParseMode -> String -> Either String TH.Name
parseNameWithMode mode content =
  case resultToEither . fmap TH.toExp $ Haskell.parseExpWithMode mode content of
    Right (TH.VarE name) -> Right name
    Right (TH.ConE name) -> Right name
    Right e -> Left (show e ++ " is not a variable")
    Left err -> Left err

-- | Build 'Egison.Fixity' using 'Haskell.Fixity' from @haskell-src-exts@.
makeFixity :: Haskell.Fixity -> Egison.Fixity TH.Name
makeFixity (Haskell.Fixity assoc prec name) = fixity
  where
    fixity = Egison.Fixity (makeAssoc assoc) (Precedence prec) $ TH.toName name
    makeAssoc (Haskell.AssocRight ()) = Egison.AssocRight
    makeAssoc (Haskell.AssocLeft ()) = Egison.AssocLeft
    makeAssoc (Haskell.AssocNone ()) = Egison.AssocNone

-- | Build 'Egison.ParseFixity' using 'Egison.Fixity' to parse Haskell-style operators
makeParseFixity :: Egison.Fixity TH.Name -> Egison.ParseFixity TH.Name String
makeParseFixity fixity = Egison.ParseFixity fixity $ makeNameParser symbol
  where
    Egison.Fixity {Egison.symbol} = fixity
    printSym = PP.render . TH.to_HPJ_Doc . TH.pprName' TH.Infix
    makeNameParser s input
      | input == printSym s = Right ()
      | otherwise = Left "not an operator name"

-- | Build 'Egison.ParseMode' using 'Haskell.ParseMode' from @haskell-src-exts@.
makeParseMode ::
  Haskell.ParseMode -> Egison.ParseMode TH.Name TH.Name TH.Exp String
makeParseMode mode@Haskell.ParseMode {Haskell.parseFilename, Haskell.fixities} =
  Egison.ParseMode
    { Egison.filename = parseFilename,
      Egison.fixities = maybe [] makeParseFixities fixities,
      Egison.blockComment = Just ("{-", "-}"),
      Egison.lineComment = Just "--",
      Egison.varNameParser = parseNameWithMode mode,
      Egison.nameParser = parseNameWithMode mode,
      Egison.valueExprParser =
        resultToEither
          . fmap TH.toExp
          . Haskell.parseExpWithMode mode
    }
  where
    makeParseFixities :: [Haskell.Fixity] -> [Egison.ParseFixity TH.Name String]
    makeParseFixities = map $ makeParseFixity . makeFixity

instance Parsable Expr String ParseMode where
  parseNonGreedyWithLocation ParseMode {haskellMode, fixities} =
    parseNonGreedyWithLocation @Expr mode'
    where
      mode = makeParseMode haskellMode
      mode' = case fixities of
        Just xs -> mode {Egison.fixities = xs}
        Nothing -> mode

-- | Parse 'Expr' using 'ParseMode'.
parseExpr :: (MonadError (Errors String) m) => ParseMode -> String -> m Expr
parseExpr = parse @Expr

-- | Parse 'Expr' using 'ParseMode' with locations annotated.
parseExprL :: (MonadError (Errors String) m) => ParseMode -> String -> m ExprL
parseExprL = parseWithLocation @Expr
