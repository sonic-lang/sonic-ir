module Language.Sonic.Compiler.IR.Attribute
  ( Attrs(..)
  , AttrKey(..)
  , Attr(..)
  , AttrValue(..)
  , EntityRef(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Data.Map                       ( Map )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XWrap
                                                , XRefID
                                                )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Var
                                                , Ctor
                                                , TyCtor
                                                , TyVar
                                                , Class
                                                , Module
                                                )

newtype AttrKey = AttrKey Text
  deriving (Eq, Ord, Generic)

newtype Attrs x = Attrs (Map AttrKey (XWrap x (Attr x)))
  deriving Generic

data Attr x
  = Name
  | Value  (XWrap x (AttrValue x))
  | List   (XWrap x [XWrap x (AttrValue x)])
  | Record (XWrap x (Attrs x))
  deriving Generic

data AttrValue x
  = TextValue (XWrap x Text)
  | RefValue  (XWrap x (EntityRef x))
  deriving Generic

data EntityRef x
  = Var    (XRefID Var x)
  | Ctor   (XRefID Ctor x)
  | TyCtor (XRefID TyCtor x)
  | TyVar  (XRefID TyVar x)
  | Class  (XRefID Class x)
  | Module (XRefID Module x)
  deriving Generic
