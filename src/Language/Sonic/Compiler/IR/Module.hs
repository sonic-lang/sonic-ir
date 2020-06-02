module Language.Sonic.Compiler.IR.Module
  ( Module(..)
  )
where

import           GHC.Generics                   ( Generic )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XWrap )
import           Language.Sonic.Compiler.IR.Attribute
                                                ( Attrs )
import           Language.Sonic.Compiler.IR.Declaration
                                                ( Decl )

data Module x
  = Module
  { attrs :: XWrap x [XWrap x (Attrs x)]
  , decls :: XWrap x [XWrap x (Decl x)]
  }
  deriving Generic
