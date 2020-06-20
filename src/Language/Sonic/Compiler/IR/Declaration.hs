{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.IR.Declaration
  ( DataDecl(..)
  , ClassDecl(..)
  , InstanceDecl(..)
  , DataCtorDecl(..)
  , ClassMethodDecl(..)
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

data DataDecl x
  = DataDecl
  { attrs :: XWrap x (Attrs x)
  , name  :: XWrap x (XDefID TyCtor x)
  , vars  :: [XWrap x (TyVarBinder x)]
  , ctors :: [XWrap x (DataCtorDecl x)]
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
  , vars         :: [XWrap x (TyVarBinder x)]
  , methods      :: [XWrap x (ClassMethodDecl x)]
  -- | Note that 'BindGroup' in 'defaultBinds' may not contain corresponding type signatures in each binds
  , defaultBinds :: [XWrap x (BindGroup x)]
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
  , types   :: [XWrap x (Type x)]
  , methods :: [XWrap x (BindGroup x)]
  }
  deriving Generic
