import Data.Map as Map
import Data.List as List
import Data.Maybe as Maybe
import System.IO( Handle, FilePath, IOMode( ReadMode ), openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr )

import Control.Monad( when )
import Data.Time
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
in2 = [(1, 2, 100), (1, 5, 18), (1, 6, 15), (2, 3, 2), (3, 4, 3), (3, 9, 10), (4, 5, 4), (5, 8, 14), (6, 5, 85), (6, 7, 4), (7, 5, 2)]
in3 = [(1, 2, 100), (1, 5, 30), (1, 6, 15), (2, 3, 2), (3, 4, 3), (3, 9, 10), (4, 5, 4), (5, 8, 14), (6, 5, 85), (6, 7, 4), (7, 5, 2)]
in4 = [(1, 2, 100), (1, 5, 30), (1, 6, 15), (2, 3, 2), (3, 4, 3), (3, 9, 10), (4, 5, 4), (5, 8, 14), (6, 5, 85), (6, 7, 4), (7, 5, 2), (100, 150, 90)]
in5 = [(1,2,3)]
in0 = 
  [(1, 2, 1),
  (2,1,15)]

makeWaysOLD [] = empty
makeWaysOLD ((a, b, c):t)
  | not aIsMemberOfWayst && not bIsMemberOfWayst = Map.insert a (fromList [(b,c)]) (Map.insert b empty (makeWaysOLD t))
  | aIsMemberOfWayst && not bIsMemberOfWayst = update (\x->Just (Map.insert b c x)) a (Map.insert b empty (makeWaysOLD t)) 
  | aIsMemberOfWayst = update (\x->Just (Map.insert b c x)) a wayst
  | otherwise = Map.insert a (fromList [(b,c)]) wayst
  where 
  aIsMemberOfWayst = member a wayst
  bIsMemberOfWayst = member b wayst
  wayst = makeWaysOLD t

makeWays graph 
  = List.foldr 
    (\(a, b, c) acc -> changeWays a b c acc)
    empty                  
    graph
    where
    changeWays a b c acc 
      = if member a acc then
          if member b acc then
            update (\x->Just (Map.insert b c x)) a acc
          else
            update (\x->Just (Map.insert b c x)) a (Map.insert b empty acc)
        else
          if member b acc then
            Map.insert a (fromList [(b,c)]) acc
          else
            Map.insert a (fromList [(b,c)]) (Map.insert b empty acc)

sayPathLength from to listGraph = wlft from to mapGraph (startWayLen from mapGraph) 0 
  where
    mapGraph = makeWays listGraph
    startWayLen from ways = foldrWithKey (startWayLen' from) empty ways where
      startWayLen' from k _ ks 
        | (k == from) = ks
        | otherwise = Map.insert k (-1) ks
        
sayLen from to ways = wlft from to ways (startWayLen from ways) 0 
  where
    startWayLen from ways = foldrWithKey (startWayLen' from) empty ways where
      startWayLen' from k _ ks 
        | (k == from) = ks
        | otherwise = Map.insert k (-1) ks
      
wlft cur to ways wayLen curLenWay 
  = if cur == to then
      curLenWay
    else
      if size wayLen < 1 || (notMember cur ways) then 
        -1 
      else 
        wlft 
          (newFrom cur ways wayLen curLenWay)
          to
          ways 
          (Map.delete (newFrom cur ways wayLen curLenWay) (newWayLen cur ways wayLen curLenWay))
          (fromJust . Map.lookup (newFrom cur ways wayLen curLenWay) $ (newWayLen cur ways wayLen curLenWay) ) 
  where
    newFrom cur ways wayLen curLenWay  
      = if posibleWay' == [] then
          -1
        else
          newFrom' . minimum $ posibleWay'         
      where
        newFrom' minValue 
          = foldrWithKey 
            (\k val acc -> if val == minValue then k else acc) (-1) (newWayLen cur ways wayLen curLenWay)
        posibleWay' = elems (Map.filter (/= -1) (newWayLen cur ways wayLen curLenWay))
    newWayLen from ways wayLen curLenWay 
      = List.foldr 
        (\vert wayLen -> changeWayLen from vert ways wayLen curLenWay) 
        wayLen 
        (keys wayLen) 
    newLen from to ways wayLen curLenWay 
      = (fromJust . Map.lookup to . fromJust . Map.lookup from $ ways ) + curLenWay  
    changeWayLen from to ways wayLen curLenWay 
      = if (/=) Nothing . Map.lookup to . fromJust . Map.lookup from $ ways then 
          if (fromJust . Map.lookup to $ wayLen) == (-1)
            || (newLen from to ways wayLen curLenWay) < (fromJust (Map.lookup to wayLen)) then 
            update (\x->Just (newLen from to ways wayLen curLenWay)) to wayLen
          else 
            wayLen
        else 
          wayLen
          
          

dumpFile ::  Handle -> FilePath -> Integer -> Map Int (Map Int Int) -> IO (Map Int (Map Int Int))

dumpFile handle filename lineNumber acc
  = do
    end <- hIsEOF handle
    if not end then
      do
        line <- hGetLine handle        
        dumpFile 
          handle 
          filename 
          (lineNumber + 1) 
          (changeWays (read (head (words line))::Int) (read (head (tail (words line)))::Int) (read (head (tail (tail (words line))))::Int) acc)
    else
      return acc
    where
    changeWays a b c acc 
      = if member a acc then
          if member b acc then
            update (\x->Just (Map.insert b c x)) a acc
          else
            update (\x->Just (Map.insert b c x)) a (Map.insert b empty acc)
        else
          if member b acc then
            Map.insert a (fromList [(b,c)]) acc
          else
            Map.insert a (fromList [(b,c)]) (Map.insert b empty acc)


main :: IO ()

main = do
    hPutStr stderr "Type a filename: "
    filename <- getLine
    hPutStr stderr "Type from: "
    from <- getLine
    hPutStr stderr "Type to: "
    to <- getLine
    handle <- openFile filename ReadMode
    ways <- (dumpFile handle filename 1 empty)
    hClose handle
    
    begT <- getCurrentTime
    putStrLn ( show (sayLen (read from::Int) (read to::Int) ways))
    endT <- getCurrentTime
    
    putStrLn $ init $ show $ diffUTCTime endT begT