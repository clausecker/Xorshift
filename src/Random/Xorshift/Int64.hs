-----------------------------------------------------------------------------
--
-- Module      :  Random.Xorshift.Int64
-- Copyright   :
-- License     :  LGPL 2.0 or higher
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This module contains a 64 bit Xorshift random generator. Use this if you
--   need a 64 bit random generator, usually 'Xorshift' itself is a good choice
--   for your platform. This generator has a period of 2^64-1 bits if
--   initialized with a value different from 0. 
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Random.Xorshift.Int64 (
    Xorshift64 (..),
    newXorshift64,
    makeXorshift64,
    getInt64
  ) where

import Data.Int  (Int64)
import Data.Bits
import System.Random
import Data.Time

-- | Xorshift generator with 64 bits.
--   This Xorshift generator uses first a left shift, then a right shift and
--   again a left shift. It uses the parameters 13, 7 and 17. See the paper for
--   more details on how the parameters affect the generation of random numbers.
--
--   The generator has a periode of 2^64-1, please notice that the
--   generator has to be initialized with a value different from 0, elseway all
--   future values will be zero as well.
--
--   Please notice, that the function 'split' is not implemented and will result
--   in a runtime error.
newtype Xorshift64 = Xorshift64 Int64
  deriving (Eq,Show,Enum,Bounded)

instance RandomGen Xorshift64 where
  next a = (fromIntegral c, b) where
    b@(Xorshift64 c) = step64 a
  genRange a = (fromEnum (minBound `asTypeOf` a),
                fromEnum (maxBound `asTypeOf` a))
  split _ = error "Xorshift64 is non-splittable"
#if MIN_VERSION_random(1, 2, 0)
  genWord32 a = (fromIntegral c, b) where
    b@(Xorshift64 c) = step64 a
  genWord64 a = (fromIntegral c, b) where
    b@(Xorshift64 c) = step64 a
#endif

-- | Generates a new Xorshift64 from the current time.
newXorshift64 :: IO Xorshift64
newXorshift64 = getRandomValue >>= return . makeXorshift64

-- | Generate a new 'Xorshift64' generator. This is essentially a wrapper around
--   the constructor.
makeXorshift64 :: Integral a => a -> Xorshift64
makeXorshift64 = Xorshift64 . fromIntegral

-- | Get the raw contents of the random generator. This function is preferable
--   over direct usage of the constructor itself, as the internal representation
--   of the random generator may change in future releases.
getInt64 :: Xorshift64 -> Int64
getInt64 (Xorshift64 a) = a

-- Generates a random value from current time. We try to archieve a good result
-- by summing all information of time.
getRandomValue :: IO Integer
getRandomValue = do UTCTime (ModifiedJulianDay day) time <- getCurrentTime
                    let picosecs = truncate $ toRational time * 10^(12 :: Int)
                        dayInPs  = day * 86400 * 10^(12 :: Int)
                    return $! picosecs + dayInPs

-- Iterates the random generator for 64 bits
step64 :: Xorshift64 -> Xorshift64
step64 (Xorshift64 a) = Xorshift64 d where
  b = a `xor` (shiftL a 13)
  c = b `xor` (shiftR b  7)
  d = c `xor` (shiftL c 17)
