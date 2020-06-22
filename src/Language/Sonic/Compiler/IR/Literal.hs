module Language.Sonic.Compiler.IR.Literal
  ( Literal(..)
  )
where

import           GHC.Generics                   ( Generic )

data Literal
  = Integer Integer
  | Char Char
  deriving (Show, Eq, Generic)
