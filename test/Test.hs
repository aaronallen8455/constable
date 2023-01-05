module Test where

import           Constable
import           Data.Monoid

data Summable = Summable
  { s1 :: Int
  , s2 :: Double
  , s3 :: Integer
  } deriving (Show)

test :: Summable
test = Summable
  { s1 = 1
  , s2 = 2
  , s3 = 3
  }

-- $setup
-- >>> import Constable
-- >>> import Data.Monoid

-- $
-- >>> :{
-- getConstSafe $ Summable
--   <$> mkConst (s1 test) (Sum . fromIntegral)
--   <*> mkConst (s2 test) Sum
--   <*> mkConst (s3 test) (Sum . fromIntegral)
-- :}
-- Sum {getSum = 6.0}

-- $
-- >>> :{
-- getConstSafe $ Summable
--   <$> mkConst (s1 test) (Sum . fromIntegral)
--   <*> mkConst (s2 test) Sum
-- :}
-- <BLANKLINE>
-- ...
--     • Const used with partially applied constructor: 'Integer
--                                                       -> Summable'
-- ...

data Field rec a = Field
  { getter :: rec -> a
  , setter :: rec -> a -> rec
  }

-- $
-- >>> :{
-- let upd = getConstSafe $ Summable
--       <$> mkConstF (Field s1 (\r a -> r { s1 = a }))
--                    (\f -> Endo $ \rec -> setter f rec (getter f rec + 1)
--                    )
--       <*> mkConstF (Field s2 (\r a -> r { s2 = a }))
--                    (\f -> Endo $ \rec -> setter f rec (getter f rec + 2)
--                    )
--       <*> mkConstF (Field s3 (\r a -> r { s3 = a }))
--                    (\f -> Endo $ \rec -> setter f rec (getter f rec + 3)
--                    )
-- in appEndo upd test
-- :}
-- Summable {s1 = 2, s2 = 4.0, s3 = 6}
