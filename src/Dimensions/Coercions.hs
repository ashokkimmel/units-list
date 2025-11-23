{-# LANGUAGE Unsafe #-}
module Dimensions.Coercions (
    coerceDimension
) where

-- | Highly unsafe coercion between 'Dimension n a' and 'a', use when you don't want to map over things.
import Data.Type.Coercion (Coercion(Coercion))
import Dimensions.Data (Dimension(MkDimension))
coerceDimension :: Coercion (Dimension n a) a
coerceDimension = Coercion 
{-# INLINE coerceDimension #-}
