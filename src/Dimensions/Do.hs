{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-|
Allows for monadic programming with 'Dimension's.
I have no idea why someone would use this, as it forfiets much of the type safety benefits of 'Dimension's.
This does allow for stuff like: 
>>> import qualified Dimensions.Do as D 
>>> import Dimensions.Units 
>>> :{
>>> D.do 
>>>    x <- inv $ 3 `dim` "meter"
>>>    y <- mult 2 $ 4 `dim` "meter"
>>>   D.pure (recip x !* y ^ 2)
>>> :}
But I think you should just use normal (!*),recipD and (!^) if you want something like that, but if you really want to use monadic style, this is here for you.
-}
module Dimensions.Do (fmap,pure,return,(<*>),(>>=),(>>),join,mult,inv) where
import Prelude hiding (pure,return,(<*>),(>>=),(>>))
import Dimensions.Data (Dimension(MkDimension))
import Dimensions.Units (type (!*), Invert,type (!^))
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
-- | Multiply dimensions by fixed constants
mult :: forall n -> Dimension m a -> Dimension (m !^ n  ) a
mult _ (MkDimension a) = MkDimension a
-- | Invert dimensions
inv :: Dimension n a -> Dimension (Invert n) a
inv (MkDimension a) = MkDimension a
{-# INLINE pure #-}
{-# INLINE return #-}
{-# INLINE (<*>) #-}
{-# INLINE (>>=) #-}
{-# INLINE (>>) #-}
{-# INLINE join #-}