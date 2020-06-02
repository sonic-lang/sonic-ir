{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.IR.Pattern
  ( Pat(..)
  , PatInfix(..)
  , XParens
  , XWildcard
  , XLiteral
  , XVar
  , XCtor
  , XInfix
  , XXPat
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

type family XParens x
type family XWildcard x
type family XLiteral x
type family XVar x
type family XCtor x
type family XInfix x
type family XXPat x

data Pat x
  = Parens   !(XParens   x) (XWrap x (Pat x))
  | Wildcard !(XWildcard x)
  | Literal  !(XLiteral  x) (XWrap x Literal)
  | Var      !(XVar      x) (XWrap x (XDefID Var x))
  | Ctor     !(XCtor     x) (XWrap x (XRefID Ctor x)) (XWrap x [XWrap x (Pat x)])
  | Infix    !(XInfix    x) (XWrap x (Pat x)) (XWrap x (PatInfix x)) (XWrap x (Pat x))
  | XPat     !(XXPat     x)
  deriving Generic

newtype PatInfix x = PatInfix (XRefID Ctor x)
  deriving Generic
