{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-| This module provides an interface for the 'Const.Const' functor which facilitates
   using it safely for summarization via the applicative instance @Monoid m => Applicative (Const m)@.

   As an example, say we have a record of numbers and a function that sums up
   the fields of that record. We want to leverage the type system to ensure
   that if the record changes (field are added or removed), then the summing
   function will not compile until it is updated to match.

   As a first attempt, we might write something like this:

   @
   import Data.Functor.Const
   import Data.Monoid

   data Parts = MkParts
     { part1 :: Int
     , part2 :: Int
     , part3 :: Int
     }

   sumParts :: Parts -> Int
   sumParts parts = getSum . getConst '$'
     MkParts
       '<$>' Const (Sum $ part1 parts)
       '<*>' Const (Sum $ part2 parts)
       '<*>' Const (Sum $ part3 parts)
   @

   At first glance it looks like this accomplishes our goal, but there is a
   serious bug. If we remove a field from the record then @sumParts@ will
   indeed fail to compile, but if we add a field then it will compile without
   issue! This is because we didn't explicitly provide a type to the @Const@
   expression being constructed and so the compiler infers it to be a function
   type rather than the fully applied type that we expect. This means we must
   always remember to add explicit type signatures or type applications
   anywhere we use this pattern.

   If we instead use the 'getConstSafe' version of @getConst@ exported by this
   module then we don't need to worry about this issue because the compiler
   will complain if the inferred type of a @Const@ is for a function type.

   If you use HLint, you can go one step further and ban the use of the
   "unsafe" 'getConst' in your codebase using an HLint rule:

   @
   - functions:
     - {name: getConst, within: []}
   @
-}
module Constable
  ( getConstSafe
  , mkConst
  , mkConstF
  , Const.Const(Const)
  ) where

import           Data.Coerce (coerce)
import qualified Data.Functor.Const as Const
import           Data.Kind (Type, Constraint)
import           GHC.TypeLits (TypeError, ErrorMessage(..))

-- | A constraint enforcing that 'getConst' is only used with non-partially
-- applied types.
type family FullyApplied (a :: Type) :: Constraint where
  FullyApplied (a -> b) = TypeError (Text "Const used with partial application")
  FullyApplied a = ()

-- | Unwraps 'Const.Const'. Throws a type error if that 'Const.Const' has a
-- function type for its second argument.
getConstSafe :: FullyApplied b => Const.Const a b -> a
getConstSafe = coerce

-- | Smart constructor for 'Const.Const' that enforces that the value it contains is
-- built from a value of the result type. This provides an extra measure of type safety.
--
-- @
-- data Summable = MkSummable
--   { s1 :: Int
--   , s2 :: Double
--   , s3 :: Integer
--   } deriving (Show)
--
-- test :: Summable
-- test = Summable
--   { s1 = 1
--   , s2 = 2
--   , s3 = 3
--   }
--
-- getConstSafe '$' MkSummable
--   '<$>' mkConst (s1 test) (Sum . fromIntegral)
--   '<*>' mkConst (s2 test) Sum
--   '<*>' mkConst (s3 test) (Sum . fromIntegral)
-- @
mkConst :: a -> (a -> m) -> Const.Const m a
mkConst a toM = coerce (toM a)

-- | Smart constructor for 'Const.Const' like 'mkConst' but where the argument is the
-- result type applied to some type constructor @f@.
mkConstF :: f a -> (f a -> m) -> Const.Const m a
mkConstF fa toM = coerce (toM fa)
