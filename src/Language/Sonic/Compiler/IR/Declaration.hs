{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.IR.Declaration
  ( Decl(..)
  , DataDecl(..)
  , ClassDecl(..)
  , InstanceDecl(..)
  , XBind
  , XData
  , XClass
  , XInstance
  , XXDecl
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XRefID
                                                , XDefID
                                                , XWrap
                                                )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Var
                                                , Ctor
                                                , TyCtor
                                                , Class
                                                )
import           Language.Sonic.Compiler.IR.Type
                                                ( Type
                                                , TyVarBinder
                                                , Context
                                                )
import           Language.Sonic.Compiler.IR.Attribute
                                                ( Attrs )
import           Language.Sonic.Compiler.IR.Expression
                                                ( BindGroup )

type family XBind     x
type family XData     x
type family XClass    x
type family XInstance x
type family XXDecl    x

data Decl x
  = Bind     !(XBind     x) (XWrap x (BindDecl x))
  | Data     !(XData     x) (XWrap x (DataDecl x))
  | Class    !(XClass    x) (XWrap x (ClassDecl x))
  | Instance !(XInstance x) (XWrap x (InstanceDecl x))
  | XDecl    !(XXDecl    x)
  deriving Generic

data BindDecl x
  = BindDecl
  { attrs  :: XWrap x (Attrs x)
  , groups :: XWrap x [XWrap x (BindGroup x)]
  }
  deriving Generic

data DataDecl x
  = DataDecl
  { attrs :: XWrap x (Attrs x)
  , name  :: XWrap x (XDefID TyCtor x)
  , vars  :: XWrap x [XWrap x (TyVarBinder x)]
  , ctors :: XWrap x [XWrap x (DataCtorDecl x)]
  }
  deriving Generic

data DataCtorDecl x
  = DataCtorDecl
  { attrs :: XWrap x (Attrs x)
  , name  :: XWrap x (XDefID Ctor x)
  , type_ :: XWrap x (Type x)
  }
  deriving Generic

data ClassDecl x
  = ClassDecl
  { attrs        :: XWrap x (Attrs x)
  , context      :: XWrap x (Context x)
  , name         :: XWrap x (XDefID Class x)
  , vars         :: XWrap x [XWrap x (TyVarBinder x)]
  , methods      :: XWrap x [XWrap x (ClassMethodDecl x)]
  , defaultBinds :: XWrap x [XWrap x (BindGroup x)]
  }
  deriving Generic

data ClassMethodDecl x
  = ClassMethodDecl
  { attrs :: XWrap x (Attrs x)
  , name  :: XWrap x (XDefID Var x)
  , type_ :: XWrap x (Type x)
  }
  deriving Generic

data InstanceDecl x
  = InstanceDecl
  { attrs   :: XWrap x (Attrs x)
  , context :: XWrap x (Context x)
  , name    :: XWrap x (XRefID Class x)
  , types   :: XWrap x [XWrap x (Type x)]
  , methods :: XWrap x [XWrap x (BindGroup x)]
  }
  deriving Generic
