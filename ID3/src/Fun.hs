{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Fun
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import System.Directory
import Data.List

needle_hay :: (Eq a) => [a] -> [a] -> Bool
needle_hay xs ys = any ( isPrefixOf xs ) ( tails ys )

main= do
    print "executing main..."
    print $ needle_hay "art" "party"


