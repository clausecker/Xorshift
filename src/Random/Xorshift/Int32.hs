-----------------------------------------------------------------------------
--
-- Module      :  Random.Xorshift.Int32
-- Copyright   :
-- License     :  LGPL 2.0 or higher
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This module contains a 32 bit Xorshift random generator. Use this if you
--   need a 32 bit random generator, usually 'Xorshift' itself is a good choice
--   for your platform. This generator has a period of 2^32-1 bits if
--   initialized with a value different from 0. 
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Random.Xorshift.Int32 (
    Xorshift32 (..),
    newXorshift32,
    makeXorshift32,
    getInt32
  ) where

import Data.Int  (Int32)
import Data.Bits
import System.Random
import Data.Time

-- | Xorshift generator with 32 bits.
--   This Xorshift generator uses first a left shift, then a right shift and
--   again a left shift. It uses the parameters 13, 17 and 5. See the paper for
--   more details on how the parameters affect the generation of random numbers.
--
--   The generator has a periode of 2^32-1, please notice that the
--   generator has to be initialized with a value different from 0, elseway all
--   future values will be zero as well.
--
--   Please notice, that the function 'split' is not implemented and will result
--   in a runtime error.
newtype Xorshift32 = Xorshift32 Int32
  deriving (Eq,Show,Enum,Bounded)

instance RandomGen Xorshift32 where
  next a = (fromIntegral c, b) where
    b@(Xorshift32 c) = step32 a
  genRange a = (fromEnum (minBound `asTypeOf` a),
                fromEnum (maxBound `asTypeOf` a))
  split _ = error "Xorshift32 is non-splittable"
#if MIN_VERSION_random(1, 2, 0)
  genWord32 a = (fromIntegral c, b) where
    b@(Xorshift32 c) = step32 a
#endif

-- | Generates a new Xorshift32 from the current time.
newXorshift32 :: IO Xorshift32
newXorshift32 = getRandomValue >>= return . makeXorshift32

-- | Generate a new 'Xorshift32' generator. This is essentially a wrapper around
--   the constructor.
makeXorshift32 :: Integral a => a -> Xorshift32
makeXorshift32 = Xorshift32 . fromIntegral

-- | Get the raw contents of the random generator. This function is preferable
--   over direct usage of the constructor itself, as the internal representation
--   of the random generator may change in future releases.
getInt32 :: Xorshift32 -> Int32
getInt32 (Xorshift32 a) = a

-- Generates a random value from current time. We try to archieve a good result
-- by summing all information of time.
getRandomValue :: IO Integer
getRandomValue = do UTCTime (ModifiedJulianDay day) time <- getCurrentTime
                    let picosecs = truncate $ toRational time * 10^(12 :: Int)
                        dayInPs  = day * 86400 * 10^(12 :: Int)
                    return $! picosecs + dayInPs

-- Iterates the random generator for 32 bits
step32 :: Xorshift32 -> Xorshift32
step32 (Xorshift32 a) = Xorshift32 d where
  b = a `xor` (shiftL a 13)
  c = b `xor` (shiftR b 17)
  d = c `xor` (shiftL c  5)
