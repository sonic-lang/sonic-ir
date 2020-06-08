{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeFamilies   #-}

module Language.Sonic.Compiler.IR.Tree
  ( XRefID
  , XDefID
  , XWrap
  , Unwrap(..)
  )
where

import           Data.Kind                      ( Type )

import           Language.Sonic.Compiler.IR.EntityKind
                                                ( EntityKind )

type family XRefID (k :: EntityKind) x
type family XDefID (k :: EntityKind) x

class Unwrap f where
  unwrap :: f a -> a

-- | Wrapper of fields in IR datatypes.
type family XWrap x :: Type -> Type
