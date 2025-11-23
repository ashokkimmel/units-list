{-# LANGUAGE Safe #-}
{-# LANGUAGE GADTs #-}
module Dimensions.Coercions (
    coerceDimension
) where

-- | Small, safe wrapper around 'Data.Coerce.coerce' to centralise coercions
-- within the Dimensions library.
import Data.Type.Coercion (Coercion(Coercion))
import Data.Coerce (Coercible, coerce)
import Dimensions.Data (Dimension(MkDimension))
coerceDimension :: Coercion (Dimension n a) a
coerceDimension = coerce
{-# INLINE coerceDimension #-}
