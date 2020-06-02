{-# LANGUAGE DataKinds      #-}

module Language.Sonic.Compiler.IR.EntityKind
  ( EntityKind(..)
  , Var
  , Ctor
  , TyVar
  , TyCtor
  , Class
  , Module
  )
where

data EntityKind
  = Var
  | Ctor
  | TyVar
  | TyCtor
  | Class
  | Module

type Var = 'Var
type Ctor = 'Ctor
type TyVar = 'TyVar
type TyCtor = 'TyCtor
type Class = 'Class
type Module = 'Module
