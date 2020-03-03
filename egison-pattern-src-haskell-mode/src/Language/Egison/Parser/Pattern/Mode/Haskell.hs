-- |
--
-- Module:      Language.Egison.Parser.Pattern.Mode.Haskell
-- Description: Parser for Egison pattern expressions in Haskell source code
-- Stability:   experimental
--
-- A parser for Egison pattern expressions in Haskell source code.

module Language.Egison.Parser.Pattern.Mode.Haskell
  (
  -- * Parsers
    Expr
  , ExprL
  , ParseMode(..)
  , parseExpr
  , parseExprL
  -- * Converting @haskell-src-exts@'s entities
  , makeParseMode
  , makeFixity
  , makeParseFixity
  )
where

import           Data.Char                      ( isUpper )
import           Data.Maybe                     ( mapMaybe )
import           Data.Functor                   ( void )
import           Control.Monad.Except           ( MonadError )

import           Language.Haskell.Exts.Syntax   ( QName(..)
                                                , QOp(..)
                                                , Exp(..)
                                                , Name(..)
                                                , Exp
                                                )
import           Language.Haskell.Exts.SrcLoc   ( SrcSpanInfo )
import qualified Language.Haskell.Exts.Fixity  as Haskell
                                                ( Fixity(..) )
import qualified Language.Haskell.Exts.Syntax  as Haskell
                                                ( Assoc(..) )
import qualified Language.Haskell.Exts.Pretty  as Haskell
                                                ( prettyPrint )
import qualified Language.Haskell.Exts.Parser  as Haskell
                                                ( parseExpWithMode
                                                , ParseMode(..)
                                                , ParseResult(..)
                                                )

import qualified Language.Egison.Syntax.Pattern
                                               as Egison
                                                ( Expr )
import qualified Language.Egison.Parser.Pattern
                                               as Egison
                                                ( ExprL
                                                , ParseMode(..)
                                                , ParseFixity(..)
                                                , Fixity(..)
                                                , Associativity(..)
                                                )
import           Language.Egison.Parser.Pattern ( Precedence(..)
                                                , Parsable(..)
                                                , Errors
                                                )


-- | Type synonym of 'Egison.Expr' to be parsed in Haskell's source code.
type Expr = Egison.Expr (QName ()) (Name ()) (Exp SrcSpanInfo)

-- | Type synonym of 'Egison.ExprL' to be parsed in Haskell's source code.
type ExprL = Egison.ExprL (QName ()) (Name ()) (Exp SrcSpanInfo)

-- | Parser configuration in @egison-pattern-src-haskell-mode@.
data ParseMode
  = ParseMode {
              -- | 'Haskell.ParseMode' from @haskell-src-exts@ for our parsers to base on.
                haskellMode :: Haskell.ParseMode
              -- | List of fixities to parse infix pattern operators.
              -- If @fixities = Just xs@, @xs@ overrides fixities obtained from 'haskellMode'.
              -- Otherwise, our parsers use fixities from 'haskellMode'.
              , fixities :: Maybe [Egison.ParseFixity (QName ()) String]
              }

resultToEither :: Haskell.ParseResult a -> Either String a
resultToEither (Haskell.ParseOk a      ) = Right a
resultToEither (Haskell.ParseFailed _ e) = Left e

parseVarNameWithMode :: Haskell.ParseMode -> String -> Either String (Name ())
parseVarNameWithMode mode content =
  case resultToEither $ Haskell.parseExpWithMode mode content of
    Right (Var _ (UnQual _ name)) -> Right $ void name
    Right e                       -> Left (show e ++ " is not a variable")
    Left  err                     -> Left err

parseNameWithMode :: Haskell.ParseMode -> String -> Either String (QName ())
parseNameWithMode mode content =
  case resultToEither $ Haskell.parseExpWithMode mode content of
    Right (Var _ name) -> Right $ void name
    Right (Con _ name) -> Right $ void name
    Right e            -> Left (show e ++ " is not a name")
    Left  err          -> Left err

-- | Build 'Egison.Fixity' using 'Haskell.Fixity' from @haskell-src-exts@.
makeFixity :: Haskell.Fixity -> Egison.Fixity (QName ())
makeFixity (Haskell.Fixity assoc prec name) = fixity
 where
  fixity = Egison.Fixity (makeAssoc assoc) (Precedence prec) name
  makeAssoc (Haskell.AssocRight ()) = Egison.AssocRight
  makeAssoc (Haskell.AssocLeft  ()) = Egison.AssocLeft
  makeAssoc (Haskell.AssocNone  ()) = Egison.AssocNone

-- | Build 'Egison.ParseFixity' using 'Egison.Fixity' to parse Haskell-style operators
makeParseFixity
  :: Egison.Fixity (QName ()) -> Maybe (Egison.ParseFixity (QName ()) String)
makeParseFixity fixity =
  Egison.ParseFixity fixity . makeNameParser <$> nameOf symbol
 where
  Egison.Fixity { Egison.symbol } = fixity
  nameOf q@(Qual () _ name) = Just (q, name)
  nameOf q@(UnQual () name) = Just (q, name)
  nameOf _                  = Nothing
  makeNameParser (q, sym) | isCon sym = pparser (QConOp () q)
                          | otherwise = pparser (QVarOp () q)
  pparser op input | input == Haskell.prettyPrint op = Right ()
                   | otherwise = Left "not an operator name"
  isCon (Ident  () (c   : _)) = isUpper c
  isCon (Symbol () (':' : _)) = True
  isCon _                     = False

-- | Build 'Egison.ParseMode' using 'Haskell.ParseMode' from @haskell-src-exts@.
makeParseMode
  :: Haskell.ParseMode
  -> Egison.ParseMode (QName ()) (Name ()) (Exp SrcSpanInfo) String
makeParseMode mode@Haskell.ParseMode { Haskell.parseFilename, Haskell.fixities }
  = Egison.ParseMode
    { Egison.filename        = parseFilename
    , Egison.fixities        = maybe [] makeParseFixities fixities
    , Egison.blockComment    = Just ("{-", "-}")
    , Egison.lineComment     = Just "--"
    , Egison.varNameParser   = parseVarNameWithMode mode
    , Egison.nameParser      = parseNameWithMode mode
    , Egison.valueExprParser = resultToEither . Haskell.parseExpWithMode mode
    }
 where
  makeParseFixities
    :: [Haskell.Fixity] -> [Egison.ParseFixity (QName ()) String]
  makeParseFixities = mapMaybe $ makeParseFixity . makeFixity

instance Parsable Expr String ParseMode where
  parseNonGreedyWithLocation ParseMode { haskellMode, fixities } =
    parseNonGreedyWithLocation @Expr mode'
   where
    mode  = makeParseMode haskellMode
    mode' = case fixities of
      Just xs -> mode { Egison.fixities = xs }
      Nothing -> mode

-- | Parse 'Expr' using 'ParseMode'.
parseExpr :: MonadError (Errors String) m => ParseMode -> String -> m Expr
parseExpr = parse @Expr

-- | Parse 'Expr' using 'ParseMode' with locations annotated.
parseExprL :: MonadError (Errors String) m => ParseMode -> String -> m ExprL
parseExprL = parseWithLocation @Expr
