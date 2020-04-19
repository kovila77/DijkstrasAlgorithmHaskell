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
  | not (member a wayst) && not (member b wayst) = insert a (fromList [(b,c)]) (insert b empty (ways t))
  | member a wayst && not (member b wayst) = update (\x->Just (insert b c x)) a (insert b empty (ways t)) 
  | member a wayst = update (\x->Just (insert b c x)) a wayst
  | otherwise = insert a (fromList [(b,c)]) wayst
  where wayst = ways t

leng from ways = foldrWithKey (leng' from) empty ways where
  leng' from k _ ks 
    | (k == from) = ks
    | otherwise = insert k (-1) ks

  
wlft cur toV ways wayLen curLenWay = if newCur == toV then curLenWay else wlft newFrom toV ways newWayLen (curLenWay+(newLen cur)) where
  newFrom = g' (minimum (elems newWayLen)) where
    g' minV = foldrWithKey (\k v acc -> if v == cur then k else acc) (-1) newWayLen
  newWayLen = Data.List.foldr (\x acc -> (changeWayLen x acc)) wayLen (keys wayLen) where
    changeWayLen to wayLen = if (Map.lookup to (Map.lookup cur ways)/=Nothing) then (if ((newLen to)< (Map.lookup to wayLen)) then (update (\x->Just newLen) to wayLen) else wayLen) else wayLen
      where 
      newLen to = (Map.lookup to (Map.lookup cur ways))+(Map.lookup to wayLen)
  
newWayLen = Data.List.foldr (\x acc -> (changeWayLen x acc)) wayLen (keys wayLen) where
  changeWayLen to wayLen = if (Map.lookup to (Map.lookup cur ways)/=Nothing) then (if ((newLen to)< (Map.lookup to wayLen)) then (update (\x->Just newLen) to wayLen) else wayLen) else wayLen
    where 
    newLen from to  = (Map.lookup to (Map.lookup cur ways))+(Map.lookup to wayLen)
  
  
  
wayFrom from [] = []
wayFrom from ((a, b, c):t)
  | from == a = (b, c):(wayFrom from t)
  | otherwise = wayFrom from t


  
  