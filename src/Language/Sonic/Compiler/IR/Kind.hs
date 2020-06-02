{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.IR.Kind
  ( Kind(..)
  , XType
  , XArrow
  , XXKind
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XWrap )

type family XType  x
type family XArrow x
type family XXKind x

data Kind x
  = Type  !(XType  x)
  | Arrow !(XArrow x) (XWrap x (Kind x)) (XWrap x (Kind x))
  | XKind !(XXKind x)
  deriving Generic
