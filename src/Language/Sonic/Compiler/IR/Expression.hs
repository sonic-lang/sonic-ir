{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.IR.Expression
  ( Expr(..)
  , Bind(..)
  , BindGroup(..)
  , CaseArm(..)
  , XVar
  , XCtor
  , XLiteral
  , XApply
  , XLambda
  , XAnnotate
  , XLet
  , XCase
  , XXExpr
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
                                                )
import           Language.Sonic.Compiler.IR.Literal
                                                ( Literal )
import           Language.Sonic.Compiler.IR.Type
                                                ( Type )
import           Language.Sonic.Compiler.IR.Pattern
                                                ( Pat )
import           Language.Sonic.Compiler.IR.Attribute
                                                ( Attrs )

type family XVar x
type family XCtor x
type family XLiteral x
type family XApply x
type family XLambda x
type family XAnnotate x
type family XLet x
type family XCase x
type family XXExpr x

data Expr x
  = Var      !(XVar      x) (XWrap x (XRefID Var x))
  | Ctor     !(XCtor     x) (XWrap x (XRefID Ctor x))
  | Literal  !(XLiteral  x) (XWrap x Literal)
  | Apply    !(XApply    x) (XWrap x (Expr x)) (XWrap x (Expr x))
  | Lambda   !(XLambda   x) (XWrap x (XDefID Var x)) (XWrap x (Expr x))
  | Annotate !(XAnnotate x) (XWrap x (Expr x)) (XWrap x (Type x))
  | Let      !(XLet      x) [XWrap x (BindGroup x)] (XWrap x (Expr x))
  | Case     !(XCase     x) (XWrap x (Expr x)) (XWrap x [XWrap x (CaseArm x)])
  | XExpr    !(XXExpr    x)
  deriving Generic

data Bind x
  = Bind
  { attrs :: XWrap x (Attrs x)
  , id    :: XWrap x (XDefID Var x)
  , type_ :: Maybe (XWrap x (Type x))
  , rhs   :: XWrap x (Expr x)
  }
  deriving Generic

newtype BindGroup x = BindGroup [XWrap x (Bind x)]
  deriving Generic

data CaseArm x
  = CaseArm
  { pat   :: XWrap x (Pat x)
  , guard :: Maybe (XWrap x (Expr x))
  , body  :: XWrap x (Expr x)
  }
  deriving Generic
