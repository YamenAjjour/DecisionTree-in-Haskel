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
    exeMain
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Cars
import ID3
import Data.List.Split (splitOn)


-- Hello World
exeMain = do
    file <- readFile "../data/small_car.csv"

    let data_lines = lines file
    let tokenized_data_lines = map (splitOn ",") data_lines
    let instances = map extractLabeledInstance tokenized_data_lines

    mapM_ (putStrLn . fst) instances -- printing

    let numOfInstances = fromIntegral(length(instances))
    let b = build_tree [0..5] instances

    let att_index = classify_instance (head tokenized_data_lines) b

    print( "classified as " ++ show att_index )

    print ( print_tree b )
    putStrLn "Hello World"
