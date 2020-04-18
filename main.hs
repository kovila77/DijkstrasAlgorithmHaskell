import Data.Map as Map
in1 = 
  [(1, 2, 1), 
  (1, 5, 30), 
  (1, 6, 15), 
  (2, 3, 2), 
  (3, 4, 3), 
  (3, 9, 10), 
  (4, 5, 4), 
  (5, 8, 14), 
  (6, 5, 85), 
  (6, 7, 4), 
  (7, 5, 2)]
in2 = [(1, 2, 100), (1, 5, 30), (1, 6, 15), (2, 3, 2), (3, 4, 3), (3, 9, 10), (4, 5, 4), (5, 8, 14), (6, 5, 85), (6, 7, 4), (7, 5, 2)]
in3 = 
  [(1, 2, 1),
  (2,1,15)]

ways [] = empty
ways ((a, b, c):t)
  | member a (ways t) && member b (ways t) = update (\x->Just (insert b c x)) a (ways t) 
  | member a (ways t) = update (\x->Just (insert b c x)) a (insert b empty (ways t) )
  | otherwise = insert a (fromList [(b,c)]) (ways t)

ways1 [] = empty
ways1 ((a, b, c):t) = insert b (empty) (insert a (empty) (ways1 t))

wayLen from graph = delete from (verMap graph)

verMap [] = empty
verMap ((a, b, c):t) = insert b (-1) (insert a (-1) (verMap t))

wayFrom from [] = []
wayFrom from ((a, b, c):t)
  | from == a = (b, c):(wayFrom from t)
  | otherwise = wayFrom from t


  
  