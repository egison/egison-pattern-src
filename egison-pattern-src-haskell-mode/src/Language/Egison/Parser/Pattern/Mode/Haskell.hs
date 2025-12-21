-- |
--
-- Module:      Language.Egison.Parser.Pattern.Mode.Haskell
-- Description: Parser for Egison pattern expressions in Haskell source code
-- Stability:   experimental
--
-- A parser for Egison pattern expressions in Haskell source code.
module Language.Egison.Parser.Pattern.Mode.Haskell
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
import Data.Char (isUpper)
import Data.Functor (void)
import Data.Maybe (mapMaybe)
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
import Language.Haskell.Exts.Pretty qualified as Haskell
  ( prettyPrint,
  )
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Exts.Syntax
  ( Exp (..),
    Name (..),
    QName (..),
    QOp (..),
  )
import Language.Haskell.Exts.Syntax qualified as Haskell
  ( Assoc (..),
  )

-- | Type synonym of 'Egison.Expr' to be parsed in Haskell's source code.
type Expr = Egison.Expr (QName ()) (Name ()) (Exp SrcSpanInfo)

-- | Type synonym of 'Egison.ExprL' to be parsed in Haskell's source code.
type ExprL = Egison.ExprL (QName ()) (Name ()) (Exp SrcSpanInfo)

-- | Parser configuration in @egison-pattern-src-haskell-mode@.
data ParseMode
  = ParseMode
  { -- | 'Haskell.ParseMode' from @haskell-src-exts@ for our parsers to base on.
    haskellMode :: Haskell.ParseMode,
    -- | List of fixities to parse infix pattern operators.
    -- If @fixities = Just xs@, @xs@ overrides fixities obtained from 'haskellMode'.
    -- Otherwise, our parsers use fixities from 'haskellMode'.
    fixities :: Maybe [Egison.ParseFixity (QName ()) String]
  }

resultToEither :: Haskell.ParseResult a -> Either String a
resultToEither (Haskell.ParseOk a) = Right a
resultToEither (Haskell.ParseFailed _ e) = Left e

parseVarNameWithMode :: Haskell.ParseMode -> String -> Either String (Name ())
parseVarNameWithMode mode content =
  case resultToEither $ Haskell.parseExpWithMode mode content of
    Right (Var _ (UnQual _ name)) -> Right $ void name
    Right e -> Left (show e ++ " is not a variable")
    Left err -> Left err

parseNameWithMode :: Haskell.ParseMode -> String -> Either String (QName ())
parseNameWithMode mode content =
  case resultToEither $ Haskell.parseExpWithMode mode content of
    Right (Var _ name) -> Right $ void name
    Right (Con _ name) -> Right $ void name
    Right e -> Left (show e ++ " is not a name")
    Left err -> Left err

-- | Build 'Egison.Fixity' using 'Haskell.Fixity' from @haskell-src-exts@.
makeFixity :: Haskell.Fixity -> Egison.Fixity (QName ())
makeFixity (Haskell.Fixity assoc prec name) = fixity
  where
    fixity = Egison.Fixity (makeAssoc assoc) (Precedence prec) name
    makeAssoc (Haskell.AssocRight ()) = Egison.AssocRight
    makeAssoc (Haskell.AssocLeft ()) = Egison.AssocLeft
    makeAssoc (Haskell.AssocNone ()) = Egison.AssocNone

-- | Build 'Egison.ParseFixity' using 'Egison.Fixity' to parse Haskell-style operators
makeParseFixity ::
  Egison.Fixity (QName ()) -> Maybe (Egison.ParseFixity (QName ()) String)
makeParseFixity fixity = Egison.ParseFixity fixity <$> makeNameParser symbol
  where
    Egison.Fixity {Egison.symbol} = fixity
    makeNameParser q@(Qual () _ name) = Just $ pparser q name
    makeNameParser q@(UnQual () name) = Just $ pparser q name
    makeNameParser _ = Nothing
    pparser q name input
      | input == printName q name = Right ()
      | otherwise = Left "not an operator name"
    printName q name
      | isCon name = Haskell.prettyPrint $ QConOp () q
      | otherwise = Haskell.prettyPrint $ QVarOp () q
    isCon (Ident () (c : _)) = isUpper c
    isCon (Symbol () (':' : _)) = True
    isCon _ = False

-- | Build 'Egison.ParseMode' using 'Haskell.ParseMode' from @haskell-src-exts@.
makeParseMode ::
  Haskell.ParseMode ->
  Egison.ParseMode (QName ()) (Name ()) (Exp SrcSpanInfo) String
makeParseMode mode@Haskell.ParseMode {Haskell.parseFilename, Haskell.fixities} =
  Egison.ParseMode
    { Egison.filename = parseFilename,
      Egison.fixities = maybe [] makeParseFixities fixities,
      Egison.blockComment = Just ("{-", "-}"),
      Egison.lineComment = Just "--",
      Egison.varNameParser = parseVarNameWithMode mode,
      Egison.nameParser = parseNameWithMode mode,
      Egison.valueExprParser = resultToEither . Haskell.parseExpWithMode mode
    }
  where
    makeParseFixities ::
      [Haskell.Fixity] -> [Egison.ParseFixity (QName ()) String]
    makeParseFixities = mapMaybe $ makeParseFixity . makeFixity

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
