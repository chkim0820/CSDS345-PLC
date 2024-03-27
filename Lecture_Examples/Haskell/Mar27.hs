-- write merge that takes 2 lists of numbers in order and returns
-- a combined list of both inputs, in order
-- merge [3, 10, 20] [5, 5, 18] => [3, 5, 5, 10, 18, 20]
merge [] l = l
merge l [] = l
merge (h1:t1) (h2:t2)
    | h1 < h2   = h1 : merge t1 (h2:t2)
    | otherwise = h2 : merge (h1:h2) t2