module Data.Random.Distribution.Hypergeometric.Test where

import Data.Int

import Data.Random
import Data.Random.Sample (sampleFrom)
import Data.Random.Distribution.Hypergeometric

import System.Random.MWC

import Data.Vector (fromList)

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Distribution.TestSuite.QuickCheck

instance Arbitrary Seed where
  arbitrary = return . toSeed . fromList =<< vector 258

testArbitraryRandom :: (GenIO -> IO Bool) -> Seed -> Property
testArbitraryRandom f s = monadicIO $ assert =<< run (f =<< restore s)

tests :: IO [Test]
tests = return

  [ testGroup "identities"

    [ testGroup "small"

      [ testProperty "draw nil" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getSmall . getPositive) p :: Int64
        v <- sampleFrom g $ hypergeometric i i 0
        return $ v == 0

      , testProperty "draw all" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getSmall . getPositive) p :: Int64
        v <- sampleFrom g $ hypergeometric i i i
        return $ v == i

      ]

    , testGroup "large"

      [ testProperty "draw nil" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getLarge . getPositive) p :: Int64
        v <- sampleFrom g $ hypergeometric i i 0
        return $ v == 0

      , testProperty "draw all" $ \p -> testArbitraryRandom $ \g -> do
        let i = (getLarge . getPositive) p :: Int64
        v <- sampleFrom g $ hypergeometric i i i
        return $ v == i

      ]

    ]

  ]
