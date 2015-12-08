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