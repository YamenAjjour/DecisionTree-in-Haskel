-----------------------------------------------------------------------------
--
-- Module      :  ID3
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

module ID3 (
best_attribute
) where

import Cars
import Data.Ord
import Data.List


data DecisionTree a b= Leaf b -- Leafs's labels
    | Node {
        att_index ::Int, -- Attribute's index
        child :: a -> (DecisionTree a b) -- Childrens
        }

type DT_String= DecisionTree [Char] [Char]

count ::(Eq a) => a -> [a] -> Int
count x ys = length(filter (==x) ys)

agg :: (Eq a) => [a] -> [Float]

agg [] = []
agg (x:xs) = (1.0+fromIntegral(count x xs)) :(agg leftover)
    where  leftover  = filter (/=x) xs

agg_with_element [] = []
agg_with_element (x:xs) = (x,1.0+fromIntegral(count x xs)): (agg_with_element leftover)
    where  leftover  = filter (/=x) xs

log2 :: Float -> Float
log2 a = (log a)/ log(2)

count_instances_attr :: Int -> [Char]-> [LabeledCar] -> Int
count_instances_attr index value list = length ( filter (==value) attribute_column )
    where attribute_column = (map ((!! index) . snd ) list)

count_instances_attr_cls :: Int -> [Char] -> [LabeledCar] -> [Float]
count_instances_attr_cls index value list = agg ( map (fst) (filter ( (==value) . (!! index ) . snd) list))

conditioned_entropy :: Int -> [Char] -> [LabeledCar] ->Float
conditioned_entropy index value list = sum ( map (negate) (zipWith (*) conditional_probabilities ( map log2 conditional_probabilities) ) )
    where conditional_probabilities = map (/ fromIntegral(length(list)) ) (count_instances_attr_cls index value list)

conditional_entropy :: Int -> Float  -> [LabeledCar] -> Float
conditional_entropy index num [] = 0.0
conditional_entropy index num ((cls,attrs):xs) = ((fromIntegral(length(filteredInstances))/num) * (conditioned_entropy index (attrs!!index) filteredInstances) + (conditional_entropy index num rest))
    where  filteredInstances = (cls,attrs): (filter ((== (attrs !! index)) . (!! index) . snd) xs)
           rest = filter ((/= (attrs !! index)).(!! index). snd) xs

get_attributes_ces :: [Int] -> [LabeledCar] -> [(Int,Float)]
get_attributes_ces [] _ =[]
get_attributes_ces (ind:left_indices) list = (ind,(conditional_entropy ind  (fromIntegral(num)) list)):(get_attributes_ces left_indices list)
    where num = length(snd(list !! 0))

best_attribute ::  [Int] ->  [LabeledCar] ->Int
best_attribute   indices list=fst ( minimumBy (comparing snd) (get_attributes_ces indices list ))

split_data :: Int -> [LabeledCar] -> [[LabeledCar]]
split_data _ [] = []
split_data index ((cls,attrs):xs) = filtered : (split_data index rest)
    where  filtered = (cls,attrs): (filter ((== (attrs !! index)) . (!! index) . snd) xs)
           rest = filter ((/= (attrs !! index)).(!! index). snd) xs

pure_instances :: [LabeledCar] -> Bool
pure_instances [] = True
pure_instances [(_,_)] = True
pure_instances ((c1,_):(c2,attrs):xs) = (c1 == c2) && (pure_instances ((c2,attrs):xs))

label_instances :: [LabeledCar] -> [Char]
label_instances list = fst (maximumBy ( comparing snd) aggregated_classes)
    where aggregated_classes = agg_with_element ( map (fst)  list)

build_tree :: [LabeledCar] -> [Int] -> DT_String
build_tree list (indices)
    | (pure_instance list) = (Leaf label_instances(list))
    | (length(indices)==1 ) = (Leaf label_instances(list))
    | otherwise Node {att_index = best_index,child = safeLookup}
        where att_index = (best_attribute indices list)
              safeLookup




