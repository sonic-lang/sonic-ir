{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Sonic.Compiler.IR.Type
  ( Type(..)
  , TyVarBinder(..)
  , Context(..)
  , Predicate(..)
  , XVar
  , XCtor
  , XApply
  , XAnnotate
  , XForall
  , XXType
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XRefID
                                                , XDefID
                                                , XWrap
                                                )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( TyVar
                                                , TyCtor
                                                , Class
                                                )
import           Language.Sonic.Compiler.IR.Kind
                                                ( Kind )

type family XVar      x
type family XCtor     x
type family XApply    x
type family XAnnotate x
type family XForall   x
type family XXType    x

data Type x
  = Var      !(XVar      x) (XWrap x (XRefID TyVar x))
  | Ctor     !(XCtor     x) (XWrap x (XRefID TyCtor x))
  | Apply    !(XApply    x) (XWrap x (Type x)) (XWrap x (Type x))
  | Annotate !(XAnnotate x) (XWrap x (Type x)) (XWrap x (Kind x))
  | Forall   !(XForall   x) [XWrap x (TyVarBinder x)] (Maybe (XWrap x (Context x))) (XWrap x (Type x))
  | XType    !(XXType    x)
  deriving Generic

data TyVarBinder x
  = TyVarBinder
  { var  :: XWrap x (XDefID TyVar x)
  , kind :: Maybe (XWrap x (Kind x))
  }
  deriving Generic

newtype Context x = Context [XWrap x (Predicate x)]
  deriving Generic
  deriving newtype (Semigroup, Monoid)

data Predicate x
  = Class (XWrap x (XRefID Class x)) [XWrap x (Type x)]
  | Equality (XWrap x (Type x)) (XWrap x (Type x))
  deriving Generic
