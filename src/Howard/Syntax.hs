{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Howard.Syntax where

import           Control.Category
import           Control.Isomorphism.Partial.Constructors
import           Control.Isomorphism.Partial.Derived
import           Control.Isomorphism.Partial.Prim
import           Control.Isomorphism.Partial.TH
import           Control.Isomorphism.Partial.Unsafe
import           Control.Monad
import           Data.Char
import qualified Data.List                                as List
import           Data.Maybe
import qualified Data.Text                                as T
import           Prelude                                  hiding (foldl, foldl1,
                                                           id, print, pure,
                                                           (*>), (.), (<$),
                                                           (<$>), (<*), (<*>))
import           Text.Syntax.Classes
import           Text.Syntax.Combinators
-- import           Text.Syntax.Parser.Naive
import           Text.Read
import           Text.Syntax.Printer.Naive

import           Howard.Expr
import           Howard.Util




fun   = $(constructorIso 'Fun)
data_ = $(constructorIso 'Data)
defineIsomorphisms ''Expr
defineIsomorphisms ''Expr'
defineIsomorphisms ''Id
defineIsomorphisms ''Param



parseDecl :: T.Text -> Either String Decl
parseDecl = runParser declSyn


printDecl :: Decl -> Maybe String
printDecl = print declSyn


parseExpr :: T.Text -> Either String Expr
parseExpr = runParser exprSyn

printExpr :: Expr -> Maybe String
printExpr = print exprSyn



-- omittedExpr :: Expr
-- omittedExpr = Typed False omittedExpr Omitted

-- simpleExpr :: Iso (Expr' Id Expr) Expr
-- simpleExpr = papply (papply typed False) omittedExpr

-- Syntax descriptions

declSyn :: Syntax d => d Decl
declSyn
      = fun <$> binderSyn <* assignSign <*> exprSyn
    <|> data_ <$> keyword "data" *> binderSyn <* keyword "|="
        <*> statements typedIdSyn

whereSyn :: Syntax d => d a -> d [a]
whereSyn syn = keyword "where" *> many syn

exprSyn :: Syntax d => d Expr
exprSyn
    -- = app <$> termSyn <*> termSyn
      -- = funType <$> exprSyn <* funSign <*> exprSyn
      = chainl1 appSyn funSign (funType . inverse (second (commute . unit)))

appSyn :: Syntax d => d Expr
appSyn = foldl1 (expr . app) <$> many1 termSyn

funType :: Iso (Expr, Expr) Expr
funType =
    expr
    . (forall `pap` True)
    . first (oneway (\t -> Wildcard $ TypedId True t "_"))


termSyn :: Syntax d => d Expr
termSyn =
    (expr <$>)
      $ Omitted <$ keyword "_"
    <|> set <$> keyword "Set" *> natural
    <|> lam <$> lambdaSign *> paramSyn <* arrowSign <*> exprSyn
    <|> forall `pap` True <$> forallSign *> paramSyn <* commaSign <*> exprSyn
    <|> var <$> nameSyn

binderSyn :: Syntax d => d Id
binderSyn = idSyn <|> typedIdSyn

paramSyn :: Syntax d => d Param
paramSyn = wildcard <$>
           ((typedId `pap` True) . commute
            <$> wildcardSyn <* typeSign <*> exprSyn)
       <|> bound <$> typedIdSyn

wildcardSyn :: Syntax d => d Name
wildcardSyn = "_" <$ keyword "_"

idSyn :: Syntax d => d Id
idSyn = typedId `pap` False `pap` omittedTerm <$> nameSyn

typedIdSyn :: Syntax d => d Id
typedIdSyn = (typedId `pap` True) . commute
             <$> nameSyn <* typeSign <*> exprSyn

nameSyn :: Syntax d => d Name
nameSyn = subset (`notElem` ["Set"]) <$> many1 letter <* optSpace

statements :: Syntax d => d a -> d [a]
statements = (`sepBy` lineSign)

-- Syntax combinators and basics

keyword :: Syntax d => String -> d ()
keyword s = text s <* optSpace

natural :: Syntax d => d Integer
natural = readShow <$> many1 digit <* optSpace

arrowSign, assignSign, commaSign, forallSign, funSign, lambdaSign, typeSign, lineSign
    :: Syntax d => d ()
arrowSign  = keyword "=>"
assignSign = keyword ":="
commaSign  = keyword ","
forallSign = keyword "forall"
funSign    = keyword "->"
lambdaSign = keyword "\\"
typeSign   = keyword ":"
lineSign   = keyword ";"

satisfy :: Syntax d => (Char -> Bool) -> d Char
satisfy f = subset f <$> token

letter :: Syntax d => d Char
letter = satisfy isAlpha

digit :: Syntax d => d Char
digit = satisfy isDigit


-- Isomorphisms

readShow :: (Read alpha, Show alpha) => Iso String alpha
readShow  = Iso readMaybe (Just . show)


pap' :: Iso (alpha, beta) gamma -> alpha -> Iso beta gamma
pap' iso a = (iso . (inverse (ignore a) *** id)) . commute . unit

pap :: Eq alpha => Iso (alpha, beta) gamma -> alpha -> Iso beta gamma
pap iso a = (iso . (element a *** id)) . commute . unit

infix 5 <$
(<$) :: (Eq a, IsoFunctor f) => a -> f () -> f a
a <$ p = element a <$> p

foldl1 :: Iso (alpha, alpha) alpha -> Iso [alpha] alpha
foldl1 f = foldl f . inverse cons

oneway :: (alpha -> beta) -> Iso alpha beta
oneway f = Iso (Just . f) (const Nothing)

first :: Iso alpha beta -> Iso (alpha, gamma) (beta, gamma)
first = (*** id)

second :: Iso alpha beta -> Iso (gamma, alpha) (gamma, beta)
second = (id ***)

-- Parser

newtype Parser a = Parser (Rope -> Either Position (a, Rope))

data Rope = Rope { remain :: T.Text, pos :: Position }

rope :: T.Text -> Rope
rope xs = Rope xs 0

uncons :: Rope -> Maybe (Char, Rope)
uncons r = (\(c, xs) -> (c, Rope xs (pos r + 1))) `fmap` T.uncons (remain r)

type Position = Int


instance IsoFunctor Parser where
    iso <$> Parser p
        = Parser $ \r -> case p r of
        Right (a, r') -> maybe (Left $ pos r) (Right . (, r'))
                         $ apply iso a
        Left err      -> Left err

instance ProductFunctor Parser where
    Parser p <*> Parser q
        = Parser $ \r -> do
            (a, r')  <- p r
            (b, r'') <- q r'
            return ((a, b), r'')

instance Alternative Parser where
    Parser p <|> Parser q
        = Parser $ \r -> q r `plusEither` p r
    empty = Parser $ \r -> Left $ pos r

instance Syntax Parser where
    pure a = Parser $ \r -> Right (a, r)
    token  = Parser $ \r -> maybe (Left $ pos r) Right $ uncons r

runParser :: Parser a -> T.Text -> Either String a
runParser (Parser p) s = either
    (\err -> Left $ "parse error at " ++ show err)
    (Right . fst) (p $ rope s)

