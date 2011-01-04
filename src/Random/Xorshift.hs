-----------------------------------------------------------------------------
--
-- Module      :  Random.Xorshift
-- Copyright   :
-- License     :  LGPL 2.0 or higher
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Xorshift random generators. The generators are implemented according to
--   the paper <http://www.jstatsoft.org/v08/i14/paper> by George Marsaglia.
--   You can use the 'RandomGen' instances to generate random numbers easily.
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}

-- Use the C-header file to generate a platform depend Xorshift type
-- This is handy as I can use the __Xorshift macro as a data-constructor.
#include "MachDeps.h"
#if WORD_SIZE_IN_BITS == 32
#define __Xorshift Xorshift32
#else
#define __Xorshift Xorshift64
#endif

module Random.Xorshift (
    Xorshift32 (..),
    Xorshift64 (..),
    Xorshift,
    newXorshift32,
    newXorshift64,
    newXorshift,
    makeXorshift32,
    makeXorshift64,
    makeXorshift,
    getInt64,
    getInt32,
    module System.Random
  ) where

import Data.Int  (Int32,Int64)
import Data.Bits (xor,shiftL,shiftR)
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
newtype Xorshift32 = Xorshift32 Int32
  deriving (Eq,Show,Enum,Bounded)

-- | Xorshift generator with 64 bits.
--   This Xorshift generator uses first a left shift, then a right shift and
--   again a left shift. It uses the parameters 13, 7 and 17. See the paper for
--   more details on how the parameters affect the generation of random numbers.
--
--   The generator has a periode of 2^64-1, please notice that the
--   generator has to be initialized with a value different from 0, elseway all
--   future values will be zero as well.
newtype Xorshift64 = Xorshift64 Int64
  deriving (Eq,Show,Enum,Bounded)

-- | Generates a new Xorshift32 from the current time.
newXorshift32 :: IO Xorshift32
newXorshift32 = getRandomValue >>= return . makeXorshift32

-- | Generates a new Xorshift32 from the current time.
newXorshift64 :: IO Xorshift64
newXorshift64 = getRandomValue >>= return . makeXorshift64

-- | Generates a new Xorshift from the current time. This is either a synonym
--   for 'newXorshift32' or 'newXorshift64'.
newXorshift :: IO Xorshift
newXorshift = getRandomValue >>= return . makeXorshift

-- | Generate a new 'Xorshift32' generator. This is essentially a wrapper around
--   the constructor.
makeXorshift32 :: Integral a => a -> Xorshift32
makeXorshift32 = Xorshift32 . fromIntegral

-- | Generate a new 'Xorshift64' generator. This is essentially a wrapper around
--   the constructor.
makeXorshift64 :: Integral a => a -> Xorshift64
makeXorshift64 = Xorshift64 . fromIntegral

-- | Generate a new 'Xorshift' generator, regardless of whatever it's bytesize
--   is on your platform.
makeXorshift :: Integral a => a -> Xorshift
makeXorshift = __Xorshift . fromIntegral

-- | Get the raw contents of the random generator. This function is preferable
--   over direct usage of the constructor itself, as the internal representation
--   of the random generator may change in future releases.
--
--   The 'getInt32' function works for 'Xorshift32' and the 'getInt64' function
--   works for 'Xorshift64'. If you're using the 'Xorshift' type, use an 'Int'
--   type in combination with the 'random' methof of "System.Random".
getInt32 :: Xorshift32 -> Int32
getInt32 (Xorshift32 a) = a

getInt64 :: Xorshift32 -> Int32
getInt64 (Xorshift32 a) = a

instance RandomGen Xorshift32 where
  next a = (fromIntegral c, b) where
    b@(Xorshift32 c) = step32 a
  split  = error "Splitting of an Xorshift random generator is not implemented."
  genRange a = (fromEnum (minBound `asTypeOf` a),
                fromEnum (maxBound `asTypeOf` a))

instance RandomGen Xorshift64 where
  next a = (fromIntegral c, b) where
    b@(Xorshift64 c) = step64 a
  split  = error "Splitting of an Xorshift random generator is not implemented."
  genRange a = (fromEnum (minBound `asTypeOf` a),
                fromEnum (maxBound `asTypeOf` a))

-- | The type Xorshift is a wrapper around either 'Xorshift32' or 'Xorshift64',
--   depending on the bit size of your system. The idea behind this is, that you
--   may want to use this package as an uncomplicated, fast random generator,
--   but repeated conversion between different word sizes are an absolute
--   performance killer.
--
--   Thus, if you don't really care about the period of your random generator or
--   how long the result is, use the 'Xorshift' synonym, whenever possible.
type Xorshift = __Xorshift

-- Iterates the random generator for 32 bits
step32 :: Xorshift32 -> Xorshift32
step32 (Xorshift32 a) = Xorshift32 d where
  b = a `xor` (shiftL a 13)
  c = b `xor` (shiftR b 17)
  d = c `xor` (shiftL c  5)

-- Iterates the random generator for 64 bits
step64 :: Xorshift64 -> Xorshift64
step64 (Xorshift64 a) = Xorshift64 d where
  b = a `xor` (shiftL a 13)
  c = b `xor` (shiftR b  7)
  d = c `xor` (shiftL c 17)

-- Generates a random value from current time. We try to archieve a good result
-- by summing all information of time.
getRandomValue :: IO Integer
getRandomValue = do UTCTime (ModifiedJulianDay day) time <- getCurrentTime
                    let picosecs = truncate $ toRational time * 10^12
                        dayInPs  = day * 86400 * 10^12
                    return $! picosecs + dayInPs
