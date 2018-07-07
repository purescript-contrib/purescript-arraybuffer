-- Uses code originally found in the purescript-encoding library.
{-
   Copyright 2018 Andreas Schacker

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
module Test.Input where

import Data.Char.Unicode          (isPrint)
import Prelude                    ((<$>), ($), (<<<))
import Data.Array                 (filter)
import Data.String.CodeUnits      (fromCharArray, toCharArray)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)


-- When UTF8-encoding a string, surrogate code points and other non-characters
-- are simply replaced by the replacement character � (U+FFFD).
-- This entails that the `encodeUtf8` function is not injective anymore and
-- thus the desired property `decodeUtf8 <<< encodeUtf8 == id` does not hold
-- in general.
--
-- For well-formed input strings, however, we can expect the property to hold.

-- Use a newtype in order to define an `Arbitrary` instance.
newtype WellFormedInput = WellFormedInput String

-- The `Arbitrary` instance for `String` currently simply chooses characters
-- out of the first 65536 unicode code points.
-- See `charGen` in `purescript-strongcheck`.
instance arbWellFormedInput :: Arbitrary WellFormedInput where
  arbitrary = WellFormedInput <<< filterString isPrint <$> arbitrary

filterString :: (Char -> Boolean) -> String -> String
filterString f s = fromCharArray <<< filter f $ toCharArray s
