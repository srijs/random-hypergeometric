{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE
    MultiParamTypeClasses,
    FlexibleContexts,
    FlexibleInstances
  #-}
-- |
-- Module    : Statistics.Distribution.Hypergeometric.GenVar
-- Copyright : (c) 2015 Sam Rijs,
--             (c) 2005 Robert Kern,
--             (c) 1998 Ivan Frohne
-- License   : MIT
--
-- Maintainer  : srijs@airpost.net
-- Stability   : experimental
--
-- The parameters of the distribution describe /k/ elements chosen
-- from a population of /l/, with /m/ elements of one type, and
-- /l/-/m/ of the other (all are positive integers).

module Data.Random.Distribution.Hypergeometric
  ( Hypergeometric
  -- ** Constructors
  , hypergeometric
  -- ** Accessors
  , getM, getL, getK
  -- ** Variate Generation
  , hypergeometricVar
  , hypergeometricVarT
  ) where

import Data.Random.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Uniform

import Data.Random.Distribution.Hypergeometric.Impl

data Hypergeometric t = Hypergeometric { getK :: !t, getL :: !t,  getM :: !t }

-- | Constructs a hypergeometric distribution from the parameters /k/, /l/ and /m/.
--   Fails if /l/ is negative, /k/ is not in [0,/l/] or /m/ is not in [0,/l/].
hypergeometric :: (Num a, Ord a) => a -> a -> a -> Hypergeometric a
hypergeometric k l m 
  | l < 0 = error "l must not be negative"
  | m < 0 || m > l = error "m must be in [0,l]"
  | k < 0 || k > l = error "k must be in [0,l]"
  | otherwise = Hypergeometric k l m

hypergeometricVar :: (Num a, Ord a, Distribution Hypergeometric a) => a -> a -> a -> RVar a
hypergeometricVar = hypergeometricVarT

hypergeometricVarT :: (Num a, Ord a, Distribution Hypergeometric a) => a -> a -> a -> RVarT m a
hypergeometricVarT k l m = rvarT (hypergeometric k l m)

instance (Integral t) => Distribution Hypergeometric t where
  rvarT (Hypergeometric k l m) = rhyper (k, l, m)
