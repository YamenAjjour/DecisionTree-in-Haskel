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
    best_attribute,
    split_data,
    build_tree,
    print_tree,
    classify_instance
    )
    where

import Cars
import Data.Ord
import Data.List
import Data.Maybe


data DecisionTree a b =
    Leaf b -- Leafs's labels
    | Node {
        att_index ::Int, -- Attribute's index
        child :: a -> (DecisionTree a b) -- Childrens
        }

type DT_String = DecisionTree [Char] [Char]

factorial n = if n == 0 then 1 else n * factorial (n - 1)

-- recursive call
print_tree :: DT_String -> String
print_tree(Leaf b) = b
print_tree(Node att_index child ) = ( show att_index )

classify_instance :: [[Char]] -> DT_String -> Int
classify_instance attributes tree = length attributes

count ::(Eq a) => a -> [a] -> Int
count x ys = length(filter (==x) ys)

agg :: (Eq a) => [a] -> [Float]

-- agg [1,2,2,3,3,3,1] -> [2.0,2.0,3.0]
agg [] = []
agg (x:xs) = (1.0+fromIntegral(count x xs)) :(agg leftover)
    where  leftover  = filter (/=x) xs

agg_with_element [] = []
agg_with_element (x:xs) = (x,1.0+fromIntegral(count x xs)): (agg_with_element leftover)
    where  leftover  = filter (/=x) xs

log2 :: Float -> Float
log2 a = (log a)/ log(2)

-- number of instances with same attribute value
count_instances_attr :: Int -> [Char]-> [LabeledCar] -> Int
count_instances_attr index attr_value list = length ( filter (==attr_value) attribute_column )
    where attribute_column = (map ((!! index) . snd ) list)

-- number of instances with same attribute value and same class [C(A_0 && B), C(A_1 && B), ..., C(A_n && B)]
count_instances_attr_cls :: Int -> [Char] -> [LabeledCar] -> [Float]
count_instances_attr_cls index value list = agg ( map (fst) (filter ( (==value) . (!! index ) . snd) list))

-- H(A|B_j)
conditioned_entropy :: Int -> [Char] -> [LabeledCar] -> Float
conditioned_entropy index attr_value l_instances = sum ( map (negate) (zipWith (*) conditional_probabilities ( map log2 conditional_probabilities) ) )
    where conditional_probabilities = map (/ num_instances_attr) (count_instances_attr_cls index attr_value l_instances)
            where num_instances_attr = fromIntegral ( length l_instances )

-- H(A|B)
conditional_entropy :: Int -> Float  -> [LabeledCar] -> Float
conditional_entropy index num [] = 0.0
conditional_entropy index num ((cls,attrs):xs) = ((fromIntegral(length(filtered_instances))/num) * (conditioned_entropy index (attrs!!index) filtered_instances) + (conditional_entropy index num rest))
    where  filtered_instances = (cls,attrs): (filter ((== (attrs !! index)) . (!! index) . snd) xs)
           rest = filter ((/= (attrs !! index)).(!! index). snd) xs

-- get attribute indices and there cond. entropy
get_attributes_ces :: [Int] -> [LabeledCar] -> [(Int,Float)]
get_attributes_ces [] _ =[]
get_attributes_ces (ind:left_indices) l_instances = (ind,(conditional_entropy ind num_instances l_instances)):(get_attributes_ces left_indices l_instances)
    where num_instances = fromIntegral(length(l_instances))

-- get index of attribute wih lowest  conditional entropy
best_attribute ::  [Int] ->  [LabeledCar] ->Int
best_attribute   indices list=fst ( minimumBy (comparing snd) (get_attributes_ces indices list ))

-- splitting instances by attribute(index)
split_data :: Int -> [LabeledCar] -> [([Char],[LabeledCar])]
split_data _ [] = []
split_data index ((cls,attrs):xs) = (attr_value,filtered) : (split_data index rest)
    where  attr_value = (attrs !! index)
           filtered =(cls,attrs): (filter ((== attr_value) . (!! index) . snd) xs)
           rest = filter ((/= attr_value).(!! index). snd) xs


-- return true if all instances have the same class
pure_instances :: [LabeledCar] -> Bool
pure_instances [] = True
pure_instances [(_,_)] = True
pure_instances ((c1,_):(c2,attrs):xs) = (c1 == c2) && (pure_instances ((c2,attrs):xs))

-- return the most common class label
label_instances :: [LabeledCar] -> [Char]
label_instances list = fst (maximumBy ( comparing snd) aggregated_classes)
    where aggregated_classes = agg_with_element ( map (fst)  list)

build_tree :: [Int] -> [LabeledCar] -> DT_String
build_tree indices l_instances
    | pure_instances l_instances = Leaf ( label_instances l_instances )
    | (length(indices)==1 ) = Leaf ( label_instances l_instances )
    | otherwise = Node {att_index = best_index, child = safeLookup }
        where
            best_index = (best_attribute indices l_instances)
            splitted_data = split_data best_index l_instances
            left_indices = ( filter (/= best_index ) indices )
            splitted_data_without_attr = (map (snd) splitted_data )
            attrs = (map (fst) splitted_data )
            children = map ( build_tree left_indices ) splitted_data_without_attr
            children_with_attr = zip attrs children
            safeLookup param = fromJust ( lookup param children_with_attr )






