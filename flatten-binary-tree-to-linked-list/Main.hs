data Tree = Node Tree Int Tree | Empty deriving (Show, Eq)

--       1
--   2       5
-- 3   4   6    7
t1 :: Tree
t1 = Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 1 (Node (Node Empty 6 Empty) 5 (Node Empty 7 Empty))

--         1
--      2     4
--    3         5
t2 :: Tree
t2 = Node (Node (Node Empty 3 Empty) 2 Empty) 1 (Node Empty 4 (Node Empty 5 Empty))

--           1
--         2
--       3
--     4
--   5
t3 :: Tree
t3 = Node (Node (Node (Node (Node Empty 5 Empty) 4 Empty) 3 Empty) 2 Empty) 1 Empty

flatten :: Tree -> [Int]
flatten Empty = []
flatten (Node lt x rt) = [x] ++ flatten lt ++ flatten rt

flatten' :: Tree -> Tree
flatten' Empty = Empty
flatten' (Node lt x rt) = Node Empty x (append (flatten' lt) (flatten' rt))

append :: Tree -> Tree -> Tree
append Empty t = t
append t Empty = t
append (Node lt x rt) t = Node lt x (append rt t)