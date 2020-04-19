import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe
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
  | not (member a wayst) && not (member b wayst) = Map.insert a (fromList [(b,c)]) (Map.insert b empty (ways t))
  | member a wayst && not (member b wayst) = update (\x->Just (Map.insert b c x)) a (Map.insert b empty (ways t)) 
  | member a wayst = update (\x->Just (Map.insert b c x)) a wayst
  | otherwise = Map.insert a (fromList [(b,c)]) wayst
  where wayst = ways t

leng from ways = foldrWithKey (leng' from) empty ways where
  leng' from k _ ks 
    | (k == from) = ks
    | otherwise = Map.insert k (-1) ks

  
wlft cur toV ways wayLen curLenWay 
  = if cur == toV then
      curLenWay
    else
      if size wayLen < 1 then 
        -1 
      else 
        wlft 
          (newFrom cur toV ways wayLen curLenWay)
          toV 
          ways 
          (Map.delete (newFrom cur toV ways wayLen curLenWay) (newWayLen cur ways wayLen curLenWay))
          (fromJust . Map.lookup (newFrom cur toV ways wayLen curLenWay) $ (newWayLen cur ways wayLen curLenWay) ) 
newFrom cur toV ways wayLen curLenWay  
  = newFrom' . minimum . elems $ (Map.filter (/= -1) (newWayLen cur ways wayLen curLenWay)) where
    newFrom' minV 
      = foldrWithKey 
        (\k v acc -> if v == minV then k else acc) (-1) (newWayLen cur ways wayLen curLenWay)
newWayLen from ways wayLen curLenWay 
  = List.foldr 
    (\x acc -> changeWayLen from x ways acc curLenWay) 
    wayLen 
    (keys wayLen) 
newLen from to ways wayLen curLenWay 
  = (fromJust . Map.lookup to . fromJust . Map.lookup from $ ways ) + curLenWay  
changeWayLen from to ways wayLen curLenWay 
  = if (/=) Nothing . Map.lookup to . fromJust . Map.lookup from $ ways then 
      if ((fromJust . Map.lookup to $ wayLen) == (-1)) 
        || ((newLen from to ways wayLen curLenWay) < (fromJust (Map.lookup to wayLen))) then 
        update (\x->Just (newLen from to ways wayLen curLenWay)) to wayLen
      else 
        wayLen
    else 
      wayLen

  
wayFrom from [] = []
wayFrom from ((a, b, c):t)
  | from == a = (b, c):(wayFrom from t)
  | otherwise = wayFrom from t


  
  