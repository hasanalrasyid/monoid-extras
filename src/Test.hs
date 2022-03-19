{-# LANGUAGE StandaloneDeriving #-}

module Test where

import Test.QuickCheck
import Control.Applicative

import Data.Monoid.Cut
import Data.Semigroup

deriving instance Eq a => Eq (Cut a)

type S = Sum Int

prop_idL :: Cut S -> Bool
prop_idL c = mempty <> c == c

prop_idR :: Cut S -> Bool
prop_idR c = c <> mempty == c

prop_mappend_assoc :: Cut S -> Cut S -> Cut S -> Bool
prop_mappend_assoc a b c = (a <> b) <> c == a <> (b <> c)
