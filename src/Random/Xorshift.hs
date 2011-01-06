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
--   This module only exports the Random generator that matches your word size,
--   if you want to use a specific length, try "Random.Xorshift.Int32" and
--   "Random.Xorshift.Int64" instead.
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}

module Random.Xorshift (
    Xorshift,
    newXorshift,
    makeXorshift,
    module System.Random
  ) where

import System.Random
import Data.Time

-- Use the C-header file to generate a platform depend Xorshift type
-- This is handy as I can use the __Xorshift macro as a data-constructor.
#include "MachDeps.h"
#if WORD_SIZE_IN_BITS == 32
#define __Xorshift Xorshift32
import Random.Xorshift.Int32
#else
#define __Xorshift Xorshift64
import Random.Xorshift.Int64
#endif

-- | Generates a new Xorshift from the current time. This is either a synonym
--   for 'newXorshift32' or 'newXorshift64'.
newXorshift :: IO Xorshift
newXorshift = getRandomValue >>= return . makeXorshift

-- | Generate a new 'Xorshift' generator, regardless of whatever it's bytesize
--   is on your platform.
makeXorshift :: Integral a => a -> Xorshift
makeXorshift = __Xorshift . fromIntegral

-- | The type Xorshift is a wrapper around either 'Xorshift32' or 'Xorshift64',
--   depending on the bit size of your system. The idea behind this is, that you
--   may want to use this package as an uncomplicated, fast random generator,
--   but repeated conversion between different word sizes are an absolute
--   performance killer.
--
--   Thus, if you don't really care about the period of your random generator or
--   how long the result is, use the 'Xorshift' synonym, whenever possible.
type Xorshift = __Xorshift

-- Generates a random value from current time. We try to archieve a good result
-- by summing all information of time.
getRandomValue :: IO Integer
getRandomValue = do UTCTime (ModifiedJulianDay day) time <- getCurrentTime
                    let picosecs = truncate $ toRational time * 10^(12 :: Int)
                        dayInPs  = day * 86400 * 10^(12 :: Int)
                    return $! picosecs + dayInPs
