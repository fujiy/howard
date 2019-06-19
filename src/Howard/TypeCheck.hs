{-# LANGUAGE LambdaCase #-}

module Howard.TypeCheck where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Maybe

import           Howard.Expr
import           Howard.NonDet
import           Howard.Syntax        (printExpr)

data Envir = Envir
    { binds  :: [(Id, Expr)]
    , consts :: [Id]
    }
instance Semigroup Envir where
    Envir bx cx <> Envir by cy = Envir (bx <> by) (cx <> cy)

instance Monoid Envir where
    mempty = Envir [] []


data TypeError
    = OutOfScope Name
    | MisMatch Type Type
    | NonType Expr

printTypeError :: TypeError -> String
printTypeError = fromMaybe "unknown error" . \case
    OutOfScope n -> Just $ "Out of scope: " ++ show n
    MisMatch x y -> do
        xs <- printExpr x
        ys <- printExpr y
        return $ "Couldn't match type " ++ xs ++ " with " ++ ys
    NonType x -> do
        xs <- printExpr x
        return $ xs ++ " is not a type"


-- type Interpreter e = ExceptT e (Reader Envir)

type TypeChecker = NonDetT TypeError (Reader Envir)

typeCheckDecl :: Envir -> Decl -> Either [TypeError] [Envir]
typeCheckDecl env d = runReader (runNonDetT $ tcDecl d) env

typeCheckExpr :: Envir -> Expr -> Either [TypeError] [Type]
typeCheckExpr env e = runReader (runNonDetT $ tcExpr e) env


tcDecl :: Decl -> TypeChecker Envir
tcDecl = \case
    Fun b e -> do
        te <- tcExpr e
        t <- unify (type_ b) te
        return $ mempty { binds = [(b {type_ = t} , e)] }
    Data b cs -> do
        cenv <- mconcat <$> mapM (tcConstr b) cs
        return $ mempty { consts = [b] } <> cenv

tcConstr :: Binder -> Binder -> TypeChecker Envir
tcConstr b c = return mempty { consts = [c] }

tcExpr :: Expr -> TypeChecker Type
tcExpr (Expr e) = case e of
    Var n -> lookupType n
    Lam x b -> do
        ta <- tcParam x
        tb <- local (<> paramBinds x omittedTerm)
            $ tcExpr b
        return $ Expr $ Forall True (Wildcard $ TypedId True ta "_") tb
    Set n -> return $ Expr $ Set (n + 1)
    Forall _ x a -> do
        t <- tcParam x
        _ <- tcType a
        return $ Expr $ Set typeLevel
    _     -> return omittedTerm

tcParam :: Param -> TypeChecker Type
tcParam = \case
    Wildcard x -> tcId x
    Bound x    -> tcId x

tcId :: Id -> TypeChecker Type
tcId x = do
    _ <- tcType $ type_ x
    return $ type_ x

tcType :: Type -> TypeChecker Kind
tcType t = do
    Expr k <- tcExpr t >>= eval
    Expr <$> case k of
        Set n -> return $ Set (n + 1)
        _     -> throwError $ NonType $ Expr k

paramBinds :: Param -> Expr -> Envir
paramBinds (Wildcard _) e = mempty
paramBinds (Bound x)    e = mempty { binds = [(x, e)] }

lookupType :: Name -> TypeChecker Type
lookupType n = do
    env <- ask
    let bs = map (type_ . fst) $ filter ((== n) . name . fst) $ binds env
        cs = map type_         $ filter ((== n) . name)       $ consts env

    returns (bs <> cs) <|> throwError (OutOfScope n)

lookupBind :: Name -> TypeChecker Expr
lookupBind n = do
    bs <- map snd . filter ((== n) . name . fst) . binds <$> ask
    returns bs <|> throwError (OutOfScope n)

lookupConst :: Name -> TypeChecker ()
lookupConst n = do
    cs <- filter ((== n) . name) . consts <$> ask
    when (null cs) $ throwError (OutOfScope n)

unify :: Type -> Type -> TypeChecker Type
unify (Expr a) (Expr b) = Expr <$> case (a, b) of
    (Var n,   Var m)   | n == m -> return $ Var n
    (Set n,   Set m)   | n == m -> return $ Set n
    (_,       Omitted)          -> return a
    (Omitted, _)                -> return b
    (_,       _)                -> throwError $ MisMatch (Expr a) (Expr b)

eval :: Expr -> TypeChecker Expr
eval (Expr a) = case a of
    Var n -> lookupBind n
         <|> Expr (Var n) <$ lookupConst n

