{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Dimensions.Do (fmap,pure,return,(<*>),(>>=),(>>),join) where
import Prelude hiding (pure,return,(<*>),(>>=),(>>))
import Dimensions.Data (Dimension(MkDimension))
import Dimensions.Units (type (!*))
pure,return :: b -> Dimension '[] b
pure = MkDimension
return = MkDimension

(<*>) :: Dimension a (x -> y) -> Dimension b x -> Dimension (a !* b) y
(MkDimension f) <*> (MkDimension a) = MkDimension (f a)
(>>=) :: Dimension a x -> (x -> Dimension b y) -> Dimension (a !* b) y
(MkDimension a) >>= f = let MkDimension b = f a in MkDimension b

(>>) :: Dimension a x -> Dimension b y -> Dimension (a !* b) y
(MkDimension _) >> (MkDimension b) = MkDimension b

join :: Dimension a (Dimension b y) -> Dimension (a !* b) y
join (MkDimension (MkDimension b)) = MkDimension b
{-# INLINE pure #-}
{-# INLINE return #-}
{-# INLINE (<*>) #-}
{-# INLINE (>>=) #-}
{-# INLINE (>>) #-}
{-# INLINE join #-}