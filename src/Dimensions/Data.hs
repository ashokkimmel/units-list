{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe #-}
module Dimensions.Data (Dimension(..)) where 
import GHC.TypeLits (KnownSymbol)
import Data.Kind (Type)
import Dimensions.Printer (Print)
import Dimensions.GetTermLevel qualified as TT 
import Dimensions.TypeLevelInt (Int')
import Data.Functor.Apply (Apply(..))
type role Dimension nominal representational
type Dimension :: forall k. [(k,Int')] -> Type -> Type 
newtype Dimension a b = MkDimension b
    deriving stock (Eq,Ord,Functor)

instance (Show b,KnownSymbol (Print dim)) => Show (Dimension dim b) where
    show (MkDimension a) =  show a ++ ' ' : TT.symbolVal (Print dim)

instance Apply (Dimension dim) where
    (MkDimension f) <.> (MkDimension a) = MkDimension (f a)
    liftF2 f (MkDimension a) (MkDimension b) = MkDimension $ f a b
instance Bind (Dimension dim) where
    MkDimension a >>- f = f a
    join (MkDimension b) = b

instance Num a => Num (Dimension '[] a) where 
    MkDimension a + MkDimension b = MkDimension (a + b)
    MkDimension a - MkDimension b = MkDimension (a - b)
    MkDimension a * MkDimension b = MkDimension (a * b)
    negate (MkDimension a) = MkDimension (negate a)
    abs (MkDimension a) = MkDimension (abs a)
    signum (MkDimension a) = MkDimension (signum a)
    fromInteger n = MkDimension (fromInteger n)

instance Real a => Real (Dimension '[] a) where
    toRational (MkDimension a) = toRational a

instance Fractional a => Fractional (Dimension '[] a) where
    MkDimension a / MkDimension b = MkDimension (a / b)
    fromRational r = MkDimension (fromRational r)
instance Enum a => Enum (Dimension '[] a) where
    toEnum n = MkDimension (toEnum n)
    fromEnum (MkDimension a) = fromEnum a
    succ (MkDimension a) = MkDimension (succ a)
    pred (MkDimension a) = MkDimension (pred a)
    enumFrom (MkDimension a) = fmap MkDimension (enumFrom a)
    enumFromTo (MkDimension a) (MkDimension b) = fmap MkDimension (enumFromTo a b)
    enumFromThen (MkDimension a) (MkDimension b) = fmap MkDimension (enumFromThen a b)
    enumFromThenTo (MkDimension a) (MkDimension b) (MkDimension c) = fmap MkDimension (enumFromThenTo a b c)
instance Integral a => Integral (Dimension '[] a) where
    quot (MkDimension a) (MkDimension b) = MkDimension (quot a b)
    rem (MkDimension a) (MkDimension b) = MkDimension (rem a b)
    div (MkDimension a) (MkDimension b) = MkDimension (div a b)
    mod (MkDimension a) (MkDimension b) = MkDimension (mod a b)
    quotRem (MkDimension a) (MkDimension b) = let (x,y) = quotRem a b in (MkDimension x, MkDimension y)
    divMod (MkDimension a) (MkDimension b) = let (x,y) = divMod a b in (MkDimension x, MkDimension y)
    toInteger (MkDimension a) = toInteger a
instance RealFrac a => RealFrac (Dimension '[] a) where
    properFraction (MkDimension a) = let (n,fr) = properFraction a in (n, MkDimension fr)
    truncate (MkDimension a) = truncate a
    round (MkDimension a) = round a
    ceiling (MkDimension a) = ceiling a
    floor (MkDimension a) = floor a/