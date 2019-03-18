
module Rotation where

import Base
import Check

--to rotate 3 cells from any part of the field
rotateCells1 :: Key -> Key -> Key -> Field -> Field
rotateCells1 key1 key2 key3 field = swapCells key1 key3 (swapCells key1 key2 field)

rotateCells2 :: Key -> Key -> Key -> Field -> Field
rotateCells2 key1 key2 key3 field = rotateCells1 key1 key2 key3 (rotateCells1 key1 key2 key3 field)

--to swap 2 cells from any part of the field
swapCells :: Key -> Key -> Field -> Field
swapCells key1 key2 field@(t, (a, b)) = helper (getValByKey key1 t) (getValByKey key2 t) key1 key2 field

--helper for swap
helper :: Value -> Value -> Key -> Key -> Field-> Field
helper val1 val2 key1 key2 field = addCell key2 val1 (deleteCell key2 (addCell key1 val2 (deleteCell key1 field)))

--add deleted cell 
addCell :: Key -> Value-> Field -> Field
addCell key val (k, (a, b)) =  ([(key, val)] ++ k, (a, b))

--delete cell
deleteCell :: Key -> Field -> Field
deleteCell key (k, (a, b)) = (filter (((/=) key) . fst) k, (a, b))

---------------------------------------------------------------------------

--NEED--
addNewNodes :: [Int] -> Field -> Field
addNewNodes randInts fld = addNewNodes' 1 randInts fld


addNewNodes' :: Int -> [Int] -> Field -> Field
addNewNodes' i rand fld@(tb, (n, m)) | i == n + 1 = fld
                                     | otherwise = addNewNodes' (i + 1) rand' fld' where
                                        (fld', rand') = restoreStr i 1 rand fld


restoreStr :: Int -> Int -> [Int] -> Field -> (Field, [Int])
restoreStr i j rand@(x:xs) fld@(tb,(n,m)) | j == m + 1 = (fld, rand)
                                          | otherwise = if getValByKey (i, j) tb == 0 then 
                                                          restoreStr i (j + 1) xs (addCell (i, j) x (deleteCell (i, j) fld)) else
                                                          restoreStr i (j + 1) rand fld

--changeValue' :: Key -> Int -> Field -> Field
--changeValue' key val field =  addCell key val (deleteCell key field)

---------------------------------------------------------------------------------------------------------------
--NEED--
isCrashFinished :: Field -> Bool
isCrashFinished f = null $ getRemovingNodes f

type Score = Int
--NEED--
removeWinParts :: Field -> Score -> (Field, Score)
removeWinParts fld score = removeWinParts' fld (getRemovingNodes fld) score

removeWinParts' :: Field -> [[Key]] -> Score -> (Field, Score)
removeWinParts' fld [] score = (fld, score)
removeWinParts' fld ([k1, k2, k3] : xs) score = removeWinParts' (deleteTriple k1 k2 k3 fld) xs (score + 1)

getRemovingNodes :: Field -> [[Key]]
getRemovingNodes fld@(tb, (n, m)) = filter (valCheckerFun tb) [[(i1, j1), (i2, j2), (i3, j3)] | i1 <- [1..n], j1 <- [1..m], 
                                                                                       i2 <- [(i1 - 1)..(i1 + 1)], j2 <- [(j1 - 1)..(j1 + 1)], 
                                                                                       i3 <- [(i1 - 1)..(i1 + 1)], j3 <- [(j1 - 1)..(j1 + 1)], 
                                                                                       userRotationCheck (i1, j1) (i2, j2) (i3, j3) fld]

valCheckerFun :: Table -> [Key] -> Bool
valCheckerFun tb [k1, k2, k3] = v1 == v2 && v2 == v3 && v1 == v3 where 
                                        v1 = getValByKey k1 tb
                                        v2 = getValByKey k2 tb
                                        v3 = getValByKey k3 tb
valCheckerFun _ _ = False


deleteTriple :: Key -> Key -> Key -> Field -> Field
deleteTriple k1 k2 k3 fld = changeValue k1 (changeValue k2 (changeValue k3 fld))
----------------------------------------------------------------------------------------------------------------

changeValue :: Key -> Field -> Field
changeValue key field =  addCell key (0) (deleteCell key field)


-- printField (addNewNodes [1,1..] (avalanche (removeWinParts (createField 10 10 (cycle [1,1,3,4,5,6,7,8,9])))))
-- printField (avalanche (removeWinParts (createField 10 10 (cycle [1,1,3,4,5,6,7,8,9]))))
-- printField (removeWinParts (createField 10 10 (cycle [1,1,3,4,5,6,7,8,9])))





