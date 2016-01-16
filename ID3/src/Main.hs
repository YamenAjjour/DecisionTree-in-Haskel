{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Cars
import ID3
import Data.List.Split (splitOn)

-- Simple function to create a hello message.
hello s = "Hello " ++ s

-- Tell QuickCheck that if you strip "Hello " from the start of
-- hello s you will be left with s (for any s).
prop_hello s = stripPrefix "Hello " (hello s) == Just s

-- Hello World
exeMain = do
    file <- readFile "../data/car.csv"
    let data_lines = lines file
    let tokenized_data_lines = map (splitOn ",") data_lines
    let instances = map extractLabeledInstance tokenized_data_lines
    mapM_ (putStrLn . fst) instances -- printing
    let numOfInstances = fromIntegral(length(instances))
    let a = (best_attribute [0..5] instances)
    print(a)
    putStrLn "Hello Arabs"

-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

