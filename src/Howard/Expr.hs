{-# LANGUAGE DuplicateRecordFields #-}

module Howard.Expr where




data Id = TypedId
    { annotated :: Bool
    , type_     :: Type
    , name      :: Name
    }
    deriving (Eq, Show)

data Param = Wildcard Id
           | Bound Id
    deriving (Eq, Show)

type Type   = Expr
type Kind   = Type
type Binder = Id

data Expr
    -- Values
    = Var Name
    | App Expr Expr
    | Lam Param Expr

    -- Types
    | Forall Explicitness Param Expr
    | Set Level

    | Omitted
    deriving (Eq, Show)

type Explicitness = Bool


type Level        = Integer

emptySetLevel, typeLevel, kindLevel :: Level
emptySetLevel = 0
typeLevel     = 1
kindLevel     = 2


type Name = String

data Decl
    = Fun Binder Expr
    | Data Binder [Binder]

data SubSpace = SubSpace Binder [Param] [Binder]
