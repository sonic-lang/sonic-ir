{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.IR.Type
  ( Type(..)
  , TypeInfix(..)
  , TyVarBinder(..)
  , Context(..)
  , Predicate(..)
  , XParens
  , XVar
  , XCtor
  , XApply
  , XInfix
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

type family XParens   x
type family XVar      x
type family XCtor     x
type family XApply    x
type family XInfix    x
type family XAnnotate x
type family XForall   x
type family XXType    x

data Type x
  = Parens   !(XParens   x) (XWrap x (Type x))
  | Var      !(XVar      x) (XWrap x (XRefID TyVar x))
  | Ctor     !(XCtor     x) (XWrap x (XRefID TyCtor x))
  | Apply    !(XApply    x) (XWrap x (Type x)) (XWrap x (Type x))
  | Infix    !(XInfix    x) (XWrap x (Type x)) (XWrap x (TypeInfix x)) (XWrap x (Type x))
  | Annotate !(XAnnotate x) (XWrap x (Type x)) (XWrap x (Kind x))
  | Forall   !(XForall   x) (XWrap x [XWrap x (TyVarBinder x)]) (Maybe (XWrap x (Context x))) (XWrap x (Type x))
  | XType    !(XXType    x)
  deriving Generic

newtype TypeInfix x = TypeInfix (XRefID TyCtor x)
  deriving Generic

data TyVarBinder x
  = TyVarBinder
  { var  :: XWrap x (XDefID TyVar x)
  , kind :: Maybe (XWrap x (Kind x))
  }
  deriving Generic

newtype Context x = Context (XWrap x [XWrap x (Predicate x)])
  deriving Generic

data Predicate x
  = Class (XWrap x (XRefID Class x)) (XWrap x [XWrap x (Type x)])
  | Equality (XWrap x (Type x)) (XWrap x (Type x))
  deriving Generic
