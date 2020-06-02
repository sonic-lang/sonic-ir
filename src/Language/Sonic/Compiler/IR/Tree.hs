{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}

module Language.Sonic.Compiler.IR.Tree
  ( XRefID
  , XDefID
  , XWrap
  )
where

import           Language.Sonic.Compiler.IR.EntityKind
                                                ( EntityKind )

type family XRefID (k :: EntityKind) x
type family XDefID (k :: EntityKind) x

-- | Wrapper of fields in IR datatypes.
type family XWrap x a
