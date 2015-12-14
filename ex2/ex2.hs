import Data.List
square_list :: [Integer] -> [Integer]
square_list a = map (\n -> n*n)a

is_even :: Integer -> Bool
is_even a =  mod a 2 == 0

remove_even :: [Integer] -> [Integer]
remove_even array  = [a | a <- array , not (is_even a) ]

remove_even_and_square :: [Integer] -> [Integer]
remove_even_and_square a = ( square_list . remove_even ) a

re_seq_sum :: [Integer] -> Integer
re_seq_sum a = (sum . remove_even_and_square) a

distance ::(Float,Float) -> (Float,Float) ->  Float
distance (a,b) (c,d) = ((a-c)**2+(b-d)**2)**0.5

perimeter :: Float->Float->Float->Float
perimeter a b c = a+b+c

area :: Float->Float->Float->Float
area a b c = sqrt(s*(s-a)*(s-b)*(s-c))
	where s = (perimeter a b c)/2

area2 :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Float 
area2 (a,b) (c,d) (e,f) = area (distance (a,b) (c,d))  (distance (a,b) (e,f)) (distance (c,d) (e,f))

square :: Float -> Float 
square a = a**2

pythagros_check :: Float -> Float -> Float -> Bool
pythagros_check a b c = square (sorted!!0)+ square (sorted!!1) == square (sorted !!2)
	where sorted = sort [a,b,c]