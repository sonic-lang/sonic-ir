{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Sonic.Compiler.IR.Pattern
  ( Pat(..)
  , XWildcard
  , XLiteral
  , XVar
  , XCtor
  , XXPat
  , Vars(..)
  , collectVars
  , mapVars
  , replace
  )
where

import           GHC.Generics                   ( Generic )
import           Data.MonoTraversable           ( MonoFoldable(..)
                                                , MonoFunctor(..)
                                                , MonoTraversable(..)
                                                , Element
                                                )
import           Control.Monad                  ( MonadPlus(..) )

import           Language.Sonic.Compiler.IR.Tree
                                                ( XRefID
                                                , XDefID
                                                , XWrap
                                                , Unwrap(..)
                                                )
import           Language.Sonic.Compiler.IR.EntityKind
                                                ( Var
                                                , Ctor
                                                )
import           Language.Sonic.Compiler.IR.Literal
                                                ( Literal )

type family XWildcard x
type family XLiteral x
type family XVar x
type family XCtor x
type family XXPat x

data Pat x
  = Wildcard !(XWildcard x)
  | Literal  !(XLiteral  x) (XWrap x Literal)
  | Var      !(XVar      x) (XWrap x (XDefID Var x))
  | Ctor     !(XCtor     x) (XWrap x (XRefID Ctor x)) [XWrap x (Pat x)]
  | XPat     !(XXPat     x)
  deriving Generic

-- TODO: Consider recursive occurences in extension constructor/fields
newtype Vars x = Vars { unVars :: Pat x }
  deriving Generic

type instance Element (Vars x) = XWrap x (XDefID Var x)

instance Unwrap (XWrap x) => MonoFoldable (Vars x) where
  ofoldr f i (Vars (Var _ v    )) = f v i
  ofoldr f i (Vars (Ctor _ _ ps)) = foldr go i ps
    where go x acc = ofoldr f acc . Vars $ unwrap x
  ofoldr _ i _ = i
  ofoldMap f = ofoldr (mappend . f) mempty
  ofoldl' f i xs = ofoldr go id xs i where go x k acc = k $! f acc x
  ofoldr1Ex f = ofoldr1Ex f . otoList
  ofoldl1Ex' f = ofoldl1Ex' f . otoList

instance Functor (XWrap x) => MonoFunctor (Vars x) where
  omap f (Vars (Var e v    )) = Vars (Var e (f v))
  omap f (Vars (Ctor e c ps)) = Vars (Ctor e c (map g ps))
    where g = fmap (unVars . omap f . Vars)
  omap _ x = x

instance (Unwrap (XWrap x), Traversable (XWrap x)) => MonoTraversable (Vars x) where
  otraverse f (Vars (Var e v    )) = Vars . Var e <$> f v
  otraverse f (Vars (Ctor e c ps)) = Vars . Ctor e c <$> traverse g ps
    where g = traverse (fmap unVars . otraverse f . Vars)
  otraverse _ x = pure x

collectVars
  :: (MonadPlus m, Unwrap (XWrap x)) => Pat x -> m (XWrap x (XDefID Var x))
collectVars p = ofoldr (mplus . pure) mzero $ Vars p

mapVars
  :: Functor (XWrap x)
  => (XWrap x (XDefID Var x) -> XWrap x (XDefID Var x))
  -> Pat x
  -> Pat x
mapVars f = unVars . omap f . Vars

replace
  :: (Unwrap (XWrap x), Functor (XWrap x))
  => (XDefID Var x -> Bool)
  -> Pat x
  -> Pat x
  -> Pat x
replace f x (Var _ v) | f (unwrap v) = x
replace f x (Ctor e c ps) = Ctor e c (map g ps) where g = fmap (replace f x)
replace _ _ p = p
