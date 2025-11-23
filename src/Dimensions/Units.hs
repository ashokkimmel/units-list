{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoGeneralisedNewtypeDeriving #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
module Dimensions.Units (
    Dimension
    , (!*)
    , (!/)
    , (!-)
    , (!+)
    , recipD
    , negateD
    , absD
    , signumD
    , sqrtD
    , cbrtD
    , type (!*)
    , type (!/)
    , type (!^^)
    , type (!^)
    , type RT
    , type RTN
    , Replace
    , Isos
    , Delete
    , Format
    , ValidDimension
    , ValidParse
    , mkisos
    , applyPos
    , applyNeg
    , apply
    , same
    , transformPos
    , transformNeg
    , transform
    , validateDimension
    , undimension
    , dimensions
    , dimension
    , dimensionsPoly
    , dimensionPoly
    , divD
    , combineD2
    , combineInvD2
    , dimNPs
    , dimNP
    , getDimensionNP
    , getDimension 
    , dims 
    , dim 
    , rtn 
    , rt 
    , (!^)
    , (!^^)
    , inject
    , replace
    , match 
    , doN
    , ReadTypeDimension
    , ToDimension
    , FromTypeDimension
    , FromDimension
    , (!<*>)
    ) where 
import Dimensions.Printer (FromDimension,FromTypeDimension)
import Dimensions.Parser (ReadTypeDimension,ToDimension)
import qualified GHC.TypeLits as TL
import GHC.TypeLits (Symbol,Nat)
import qualified Dimensions.TypeLevelInt as TI
import Dimensions.TypeLevelInt (Int')
import Dimensions.Parser (Parse)
import Dimensions.Order (Sort,Merge)
import Data.Kind (Constraint)
import Dimensions.DimensionalMisc (Isos',Delete,UnZero,Replace',LookupD0,Invert)
import Dimensions.Data (Dimension(MkDimension))
import Data.Functor.Apply (liftF2)
import qualified Dimensions.GetTermLevel as TT
import Dimensions.Match (MatchAll,ChangeMatch,HowManyMatches,convert,unconvert)
type Replace :: k -> k -> [(k, Int')] -> [(k, Int')]
type Replace s t x = Sort (Replace' s t x)
type Isos :: [(a, a)] -> [(a, k)] -> [(k, Int')]
type Isos a b = Sort (Isos' a b)
type Format :: [(k, Int')] -> [(k, Int')]
type Format a = Sort (UnZero a)
type ValidDimension :: [(k, Int')] -> Constraint
type ValidDimension a = (a ~ Format a)
type ValidParse :: forall k. Symbol -> [(k,Int')]
type ValidParse a = Sort (Parse a)
type (!*) :: [(k,Int')] -> [(k,Int')] -> [(k,Int')]
type (!*) a b = UnZero (Merge a b)
type (!/) :: [(k,Int')] -> [(k,Int')] -> [(k,Int')]
type (!/) a b = UnZero (Merge a (Invert b))
type (!^^) :: [(a,Int')] -> Int' -> [(a,Int')]
type family (!^^) a b where 
  '[] !^^ _ = '[]
  ('(a,b)':xs) !^^ e = '(a,b TI.* e) ': xs !^^ e 
type (!^) :: [(a,Int')] -> Nat -> [(a,Int')]
type a !^ b = a !^^ ('TI.Pos b)
type RT :: [(a,Int')] ->  Int' -> [(a,Int')]
type Sqrt :: [(a,Int')] -> [(a,Int')]
type Sqrt b = RTN b 2
type Cbrt :: [(a,Int')] -> [(a,Int')]
type Cbrt b = RTN b 3


type family RT a b where 
  '[] `RT` _ = '[]
  ('(a,e)':xs) `RT` b = '(a, e TI./ b) ': xs `RT` b
type RTN :: [(a,Int')] -> Nat -> [(a,Int')]
type RTN a b = RT a ('TI.Pos b) 
-- | Raise the numeric value inside a 'Dimension' to an integral power.
-- The type-level exponent is applied to the dimension itself (so the
-- resulting 'Dimension' has each component multiplied by the given
-- exponent at the type level).
(!^^) :: Fractional n => Dimension a n -> forall b-> TT.ToInt b => Dimension (a !^^ b) n
(MkDimension a) !^^ b = MkDimension (a ^^ (TT.intval b))
infixr 8 !^^
{-# INLINE (!^^) #-}
-- | Like '!^^' but the exponent is a type-level natural number.
-- Useful when you only need non-negative integer exponents.
(!^) :: Num n => Dimension a n -> forall b-> TL.KnownNat b => Dimension (a !^ b) n
(MkDimension a) !^ b = MkDimension (a ^ (TT.natVal b))
{-# INLINE (!^) #-}
-- | Take a (possibly fractional) root of the numeric value inside a
-- 'Dimension'. The type-level 'RT' family adjusts the type to represent
-- the root (e.g. square-root, cube-root) using an 'Int'' divisor.
rt :: Floating n => Dimension a n -> forall b-> TT.ToInt b => Dimension (RT a b) n
rt (MkDimension a) b = MkDimension (a ** (recip (fromInteger (TT.intval b))))
{-# INLINE rt #-}
-- | Like 'rt' but the root degree is given as a type-level natural.
rtn :: Floating n => Dimension a n -> forall b-> TL.KnownNat b => Dimension (RTN a b) n
rtn (MkDimension a) b = MkDimension (a ** (recip (fromInteger (TT.natVal b))))
{-# INLINE rtn #-}
-- | Add the numeric values inside two dimensions with the same
-- dimension tag. This is a thin wrapper around 'liftD2'.
(!+) :: Num n => Dimension a n -> Dimension a n -> Dimension a n
(!+) = liftF2 (+)
infixl 6 !+
{-# INLINE (!+) #-}

-- | Subtract the numeric values inside two dimensions with the same
-- dimension tag.
(!-) :: Num n => Dimension a n -> Dimension a n -> Dimension a n
(!-) = liftF2 (-)
infixl 6 !-
{-# INLINE (!-) #-}
-- | Create a 'Dimension' from a parsed symbol string with the value
-- argument last. This is the shorter form used in many examples.
dim :: forall b. b -> forall a -> Dimension (ValidParse @Symbol a) b 
dim b _ = MkDimension b  
{-# INLINE dim #-}
-- | Lift 'dim' over a functor.
dims :: forall f b. Functor f => f b -> forall a ->  f (Dimension (ValidParse @Symbol a) b) 
dims b _ = fmap MkDimension b  
{-# INLINE dims #-}


-- | Negate the numeric value(s) inside a functor.
negateD :: (Num a,Functor f) => f a -> f a 
negateD = fmap negate


-- | Apply 'abs' to the numeric value(s) inside a functor.
absD :: (Num a,Functor f) => f a -> f a 
absD = fmap abs
{-# INLINE absD #-}


-- | Extract the sign of the numeric value inside a 'Dimension'. Note
-- that the result is dimensionless (type 'a').
signumD :: (Num a) => Dimension n a -> a
signumD (MkDimension a) = signum a
{-# INLINE signumD #-}
-- | Square-root the numeric value inside a 'Dimension'. The type-level
-- 'Sqrt' is applied to the dimension.
sqrtD :: Floating n => Dimension a n -> Dimension (Sqrt a) n
sqrtD (MkDimension a) = MkDimension (sqrt a)
{-# INLINE sqrtD #-}
-- | Cube-root the numeric value inside a 'Dimension'. The type-level
-- 'Cbrt' is applied to the dimension.
cbrtD :: Floating n => Dimension a n -> Dimension (Cbrt a) n
cbrtD (MkDimension a) = MkDimension (a ** (1 / 3))
{-# INLINE cbrtD #-}

-- | Create a 'Dimension' from a parsed symbol string.
dimension :: forall a -> forall b. b -> Dimension (ValidParse @Symbol a)  b
dimension _ = MkDimension
{-# INLINE dimension #-}

-- | Lift 'dimension' over a functor.
dimensions :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse @Symbol a) b)
dimensions _ = fmap MkDimension
{-# INLINE dimensions #-}

-- | Polymorphic version of 'dimension' that accepts a custom parser
-- via the 'ToDimension'/'FromDimension' machinery.
dimensionPoly :: forall a -> forall b.  b -> Dimension (ValidParse a) b
dimensionPoly _ = MkDimension
{-# INLINE dimensionPoly #-}

-- | Functor-lifted version of 'dimensionPoly'.
dimensionsPoly :: forall a -> forall f b. Functor f => f b -> f (Dimension (ValidParse a) b)
dimensionsPoly _ = fmap MkDimension
{-# INLINE dimensionsPoly #-}

-- | Create a 'Dimension' from a manually specified (non-parsed)
-- type-level 'Format' list. Use when you don't want to rely on the
-- built-in parser.
dimNP :: forall a -> forall b. b -> Dimension (Format a) b
dimNP _ = MkDimension
{-# INLINE dimNP #-}

-- | Functor-lifted version of 'dimNP'.
dimNPs :: forall a -> forall f b. Functor f => f b -> f (Dimension (Format a) b)
dimNPs _ = fmap MkDimension
{-# INLINE dimNPs #-}

-- | Repack a 'Dimension' value to its normalized 'Format' type. This
-- is a no-op at the value-level but enforces the type-level invariants
-- (ordering and non-zero exponents).
validateDimension :: Dimension a b -> Dimension (Format a) b
validateDimension (MkDimension a) = MkDimension a
{-# INLINE validateDimension #-}

-- | Combine two dimensions by applying a binary function to their
-- numeric contents and combining their type-level tags via the
-- '!*' type family.
combineD2 :: (a -> b -> c) -> Dimension tag1 a -> Dimension tag2 b -> Dimension (tag1 !* tag2) c
combineD2 f (MkDimension a) (MkDimension b) = MkDimension (f a b)
{-# INLINE combineD2 #-}
-- | Like 'combineD2' but divides the second tag from the first (using
-- the '!/' type family). Useful for operations that combine a value
-- with an inverse-tagged value.
combineInvD2 :: (a -> b -> c) -> Dimension tag1 a -> Dimension tag2 b -> Dimension (tag1 !/ tag2) c
combineInvD2 f (MkDimension a) (MkDimension b) = MkDimension (f a b)
{-# INLINE combineInvD2 #-}

-- | Multiply the numeric values inside two 'Dimension's and combine
-- their type-level tags via the '!*' family.
(!*) :: Num n => Dimension a n -> Dimension b n -> Dimension (a !* b) n
(MkDimension a) !* (MkDimension b) = MkDimension (a * b)
infixl 7 !*
{-# INLINE (!*) #-}
-- | Divide the numeric values inside two 'Dimension's and combine
-- their type-level tags via the '!/' family.
(!/) :: Fractional n => Dimension a n -> Dimension b n -> Dimension (a !/ b) n
(MkDimension a) !/ (MkDimension b) = MkDimension (a / b)
infixl 7 !/
{-# INLINE (!/) #-}
-- | Take the reciprocal of the numeric value and invert the
-- type-level tag.
recipD :: Fractional n => Dimension a n -> Dimension (Invert a) n
recipD (MkDimension a) = MkDimension $ recip a
{-# INLINE recipD #-}
-- | Integer division for dimensioned values. The result tag divides
-- the second from the first at the type level.
divD :: Integral n => Dimension a n -> Dimension b n -> Dimension (a !/ b) n
divD (MkDimension a) (MkDimension b) = MkDimension (a `div` b)
{-# INLINE divD #-}
-- | Remove the dimension wrapper; the dimension must already be
-- unitless (i.e. the '[]' tag).
undimension :: Dimension '[] a -> a
undimension (MkDimension a) = a
{-# INLINE undimension #-}
-- | Extract the numeric value from a 'Dimension' when you know the
-- parsed type-level tag (via the 'Parse' type family).
getDimension :: forall a -> Dimension (Parse a) c -> c 
getDimension _ (MkDimension c) = c
{-# INLINE getDimension #-}
-- | Extract the numeric value from a 'Dimension' when you already have
-- the type-level tag (non-parsed / non-polymorphic form).
getDimensionNP :: forall a -> Dimension a c -> c 
getDimensionNP _ (MkDimension c) = c
{-# INLINE getDimensionNP #-}
-- | Apply a function 'n' times to a value. Used internally for
-- repeated unit transformations but is still useful.
doN :: (Eq a, Num a) => (t -> t) -> a -> t -> t
doN f = go where 
    go 0 a  = a
    go x a = f (go (x - 1) a)
{-# INLINE doN #-}
-- | Replace occurrences of a symbol in the type-level tag by applying
-- the supplied function(s) the required number of times (positive or
-- negative occurrences are handled via the inverse function).
transform :: forall x a. forall s t -> TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transform s _ (fun,invfun) (MkDimension a) = let times = TT.intval (LookupD0 s x) in
    case compare times 0 of 
        EQ -> MkDimension a
        GT -> MkDimension $ doN fun times a
        LT -> MkDimension $ doN invfun (negate times) a
{-# INLINE transform #-}

-- | Like 'transform' but only for positive occurrences; requires the
-- count to be known at the type-level as a 'Nat'.
transformPos :: forall x a. forall s t -> (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transformPos s _ fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (LookupD0 s x)) in
    MkDimension $ doN fun times a
{-# INLINE transformPos #-}

-- | Like 'transformPos' but specialized to negative occurrences.
transformNeg :: forall x a. forall s t -> (TL.KnownNat (TI.ToNatural (TI.Negate (LookupD0 s x)))) => (a -> a) -> Dimension x a -> Dimension (Replace s t x) a
transformNeg s _ fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (TI.Negate (LookupD0 s x))) in
    MkDimension $ doN fun times a
{-# INLINE transformNeg #-}

-- | Replace a type-level tag without modifying the numeric value. This
-- is useful when two symbols are known to be isomorphic (e.g. different
-- string synonyms for the same unit).
same :: forall x a. forall s t -> Dimension x a -> Dimension (Replace s t x) a
same _ _ (MkDimension a) = MkDimension a
{-# INLINE same #-}

-- | Consume a tag from the dimension by applying a function the
-- appropriate number of times; useful for prefixes such as
-- "billion" which should be removed from the resulting dimension.
apply :: forall x a. forall s -> TT.ToInt (LookupD0 s x) => (a -> a, a -> a) -> Dimension x a -> Dimension (Delete s x) a
apply s (fun,invfun) (MkDimension a) = let times = TT.intval (LookupD0 s x) in
    case compare times 0 of
        EQ -> MkDimension a
        GT -> MkDimension $ doN fun times  a
        LT -> MkDimension $ doN invfun (negate times) a
{-# INLINE apply #-}

-- | 'apply' specialized to the case where the tag appears a positive
-- number of times and that count is known at the type level.
applyPos :: forall x a. forall s -> (TL.KnownNat (TI.ToNatural (LookupD0 s x))) => (a -> a) -> Dimension x a -> Dimension (Delete s x) a
applyPos s fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (LookupD0 s x)) in
    MkDimension $ doN fun times  a
{-# INLINE applyPos #-}

-- | 'apply' specialized to negative occurrences.
applyNeg :: forall x a. forall s -> (TL.KnownNat (TI.ToNatural (TI.Negate (LookupD0 s x)))) => (a -> a) -> Dimension x a -> Dimension (Delete s x) a
applyNeg s fun (MkDimension a) = let times = TT.natVal (TI.ToNatural (TI.Negate (LookupD0 s x))) in
    MkDimension $ doN fun times a
{-# INLINE applyNeg #-}
--mkisos is the same as repeated use of same
-- | Promote a dimension to an 'Isos' list; same as repeated application
-- of 'same'. Useful for using explicit isomorphism lists at the
-- type level.
mkisos :: forall y -> forall x a. Dimension x a -> Dimension (Isos y x) a
mkisos _ (MkDimension a) = MkDimension a
{-# INLINE mkisos #-}

-- | Inject a value into a larger dimension by applying a function to
-- the numeric part and adding the new tag at the type level.
inject :: (n -> n) -> forall a -> Dimension b n -> Dimension (a !* b) n
inject f _ (MkDimension a) = MkDimension (f a)
{-# INLINE inject #-}
-- | Convenience: inject without changing the numeric value.
replace :: forall a -> Dimension b n -> Dimension (a !* b) n
replace = inject id
{-# INLINE replace #-}

-- | Apply a 'MatchAll' transformation to a dimension. This can be used
-- to strip or change prefixes in a dimension according to a
-- user-supplied 'MatchAll' instance.
match :: forall x b k. forall identifier -> (MatchAll identifier k b,TT.ToInt (HowManyMatches identifier x)) => Dimension x b -> Dimension (ChangeMatch identifier x) b
match identifier (MkDimension a) = let times = TT.intval (HowManyMatches identifier x) in 
    case compare times 0 of
        EQ -> MkDimension a
        GT -> MkDimension $ doN (convert @_ @identifier) times a
        LT -> MkDimension $ doN (unconvert @_ @identifier) (negate times) a
{-# INLINE match #-}

