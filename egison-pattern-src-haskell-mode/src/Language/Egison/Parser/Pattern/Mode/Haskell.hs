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
  , parseExpr
  , parseExprWithFixities
  -- * Converting @haskell-src-exts@'s entities
  , ParseMode
  , ParseFixity
  , Fixity
  , makeHaskellMode
  , makeFixity
  , makeParseFixity
  , makeParseFixities
  )
where

import           Data.Functor                   ( void )
import           Data.Maybe                     ( mapMaybe )
import           Control.Monad.Except           ( MonadError )

import           Language.Haskell.Exts.Syntax   ( QName(..)
                                                , Exp(..)
                                                , Name(..)
                                                , ModuleName(..)
                                                , Exp
                                                )
import           Language.Haskell.Exts.SrcLoc   ( SrcSpanInfo )
import qualified Language.Haskell.Exts.Fixity  as Haskell
                                                ( Fixity(..) )
import qualified Language.Haskell.Exts.Syntax  as Haskell
                                                ( Assoc(..) )
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
                                                ( ParseMode(..)
                                                , ParseFixity(..)
                                                , Fixity(..)
                                                , Associativity(..)
                                                , parseExpr
                                                )
import           Language.Egison.Parser.Pattern ( Precedence(..)
                                                , Errors
                                                )


-- | Type synonym of 'Egison.Expr' to be parsed in Haskell's source code.
type Expr = Egison.Expr (QName ()) (Name ()) (Exp SrcSpanInfo)

-- | Type synonym of 'Egison.ParseMode' to parse 'Expr'.
type ParseMode = Egison.ParseMode (QName ()) (Name ()) (Exp SrcSpanInfo) String

-- | Type synonym of 'Egison.Fixity' to parse 'Expr'.
type Fixity = Egison.Fixity (QName ())

-- | Type synonym of 'Egison.ParseFixity' to parse 'Expr'.
type ParseFixity = Egison.ParseFixity (QName ()) String

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
makeFixity :: Haskell.Fixity -> Fixity
makeFixity (Haskell.Fixity assoc prec name) = fixity
 where
  fixity = Egison.Fixity (makeAssoc assoc) (Precedence prec) name
  makeAssoc (Haskell.AssocRight ()) = Egison.AssocRight
  makeAssoc (Haskell.AssocLeft  ()) = Egison.AssocLeft
  makeAssoc (Haskell.AssocNone  ()) = Egison.AssocNone

-- | Build 'Egison.ParseFixity' using 'Egison.Fixity' to parse Haskell-style operators
-- Note that a built-in constructor with special syntax, that is represented as 'Special' in 'QName', is just ignored here.
makeParseFixity :: Fixity -> Maybe ParseFixity
makeParseFixity fixity = Egison.ParseFixity fixity <$> makeNameParser symbol
 where
  Egison.Fixity { Egison.symbol } = fixity
  makeNameParser (UnQual () (Ident () n)) = Just $ makeIdentOpParser Nothing n
  makeNameParser (UnQual () (Symbol () n)) =
    Just $ makeSymbolOpParser Nothing n
  makeNameParser (Qual () (ModuleName () m) (Ident () n)) =
    Just $ makeIdentOpParser (Just m) n
  makeNameParser (Qual () (ModuleName () m) (Symbol () n)) =
    Just $ makeSymbolOpParser (Just m) n
  makeNameParser (Special () _) = Nothing  -- Skipping special built-in constructors
  -- TODO: Maybe we could do better here
  makeIdentOpParser mModName ident content
    | content == printed = Right ()
    | otherwise          = Left "not an operator name"
    where printed = '`' : maybe ident (++ '.' : ident) mModName ++ "`"
  makeSymbolOpParser mModName sym content
    | content == printed = Right ()
    | otherwise          = Left "not an operator name"
    where printed = maybe sym (++ '.' : sym) mModName

-- | @'makeParseFixities' = 'mapMaybe' $ 'makeParseFixity' . 'makeFixity'@
makeParseFixities :: [Haskell.Fixity] -> [ParseFixity]
makeParseFixities = mapMaybe $ makeParseFixity . makeFixity

-- | Build 'ParseMode' using 'Haskell.ParseMode' from @haskell-src-exts@.
makeHaskellMode :: Haskell.ParseMode -> ParseMode
makeHaskellMode mode@Haskell.ParseMode { Haskell.fixities } = Egison.ParseMode
  { Egison.fixities        = maybe [] makeParseFixities fixities
  , Egison.blockComment    = Just ("{-", "-}")
  , Egison.lineComment     = Just "--"
  , Egison.varNameParser   = parseVarNameWithMode mode
  , Egison.nameParser      = parseNameWithMode mode
  , Egison.valueExprParser = resultToEither . Haskell.parseExpWithMode mode
  }

-- | Parse 'Expr' using 'Haskell.ParseMode' from @haskell-src-exts@.
parseExpr
  :: MonadError (Errors String) m => Haskell.ParseMode -> String -> m Expr
parseExpr mode@Haskell.ParseMode { Haskell.parseFilename } =
  Egison.parseExpr (makeHaskellMode mode) parseFilename

-- | Parse 'Expr' using 'Haskell.ParseMode' from @haskell-src-exts@, while supplying an explicit list of 'ParseFixity'.
-- Note that fixities obtained from 'Haskell.ParseMode' are just ignored here.
parseExprWithFixities
  :: MonadError (Errors String) m
  => Haskell.ParseMode
  -> [Fixity]
  -> String
  -> m Expr
parseExprWithFixities mode@Haskell.ParseMode { Haskell.parseFilename } fixities
  = Egison.parseExpr hsModeWithFixities parseFilename
 where
  hsMode = makeHaskellMode mode
  hsModeWithFixities =
    hsMode { Egison.fixities = mapMaybe makeParseFixity fixities }
