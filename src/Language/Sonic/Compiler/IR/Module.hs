{-# LANGUAGE TypeFamilies #-}

module Language.Sonic.Compiler.IR.Module
  ( Module(..)
  , XModule
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XWrap )
import           Language.Sonic.Compiler.IR.Attribute
                                                ( Attrs )
import           Language.Sonic.Compiler.IR.Expression
                                                ( BindGroup )
import           Language.Sonic.Compiler.IR.Declaration
                                                ( DataDecl
                                                , ClassDecl
                                                , InstanceDecl
                                                )

type family XModule    x

data Module x
  = Module
  { attrs         :: [XWrap x (Attrs x)]
  , bindings      :: [XWrap x (BindGroup x)]
  , dataDecls     :: [XWrap x (DataDecl x)]
  , classDecls    :: [XWrap x (ClassDecl x)]
  , instanceDecls :: [XWrap x (InstanceDecl x)]
  , extension     :: XModule x
  }
  deriving Generic
