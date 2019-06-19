{-# LANGUAGE DuplicateRecordFields #-}

module Howard.Expr where




data Id = TypedId
    { annotated :: Bool
    , type_     :: Type
    , name      :: Name
    }

instance Eq Id where
    x == y = name x == name y

data Param = Wildcard Id
           | Bound Id
    deriving Eq

type Type   = Expr
type Kind   = Type
type Binder = Id

newtype Expr = Expr (Expr' Expr)
    deriving Eq

data Expr' a
    -- Values
    = Var Name
    | App a a
    | Lam Param a

    -- Types
    | Forall Explicitness Param a
    | Set Level

    | Omitted
    deriving Eq

type Explicitness = Bool

omittedTerm :: Expr
omittedTerm = Expr Omitted

type Level        = Integer

valueLevel, typeLevel, kindLevel :: Level
valueLevel = 0
typeLevel  = 1
kindLevel  = 2


type Name = String

data Decl
    = Fun Binder Expr
    | Data Binder [Binder]

data SubSpace = SubSpace Binder [Param] [Binder]
