-----------------------------------------------------------------------------
--
-- Module      :  Cars
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

module Cars (LabeledCar,extractLabeledInstance
) where


type LabeledInstance a b = (b ,[a]) -- a = type of attribute; b = classtype

type LabeledCar = LabeledInstance [Char] [Char]

extractLabeledInstance  :: [[Char]] -> LabeledCar
extractLabeledInstance a = (last a, init a)
