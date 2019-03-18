module Base where

type Value = Int
type Key = (Int, Int)
type Table = [(Key, Value)]
type Field = (Table, (Int, Int))

createField :: Int -> Int -> [Int] -> Field
createField n m randInts = (zip [(i, j)| i <- [1..n], j <- [1..m]] randInts, (n, m))

getValByKey :: Key -> Table -> Value
getValByKey (i, j) [] = -1
getValByKey k (x : xs) | k == fst x = snd x
                       | otherwise = getValByKey k xs



{--------------
rotateCells :: Key -> Key -> Key -> Field -> Field
rotateCells key1 key2 key3 field =  swapCells key1 key3 (swapCells key1 key2 field)

deleteCell :: Key -> Field -> Field
deleteCell key (k, (a, b)) = (filter (((/=) key) . fst) k, (a, b)) 

swapCells :: Key -> Key -> Field -> Field
swapCells key1 key2 field@(t, (a, b)) = helper (getValByKey key1 t) (getValByKey key2 t) key1 key2 field where
    helper :: Value -> Value -> Key -> Key -> Field -> Field
    helper val1 val2 key1 key2 field = addCell key2 val1 (deleteCell key2 (addCell key1 val2 (deleteCell key1 field)))

addCell :: Key -> Value-> Field -> Field
addCell key val (k, (a, b)) =  ([(key, val)] ++ k, (a, b)
-------------------------------------}
