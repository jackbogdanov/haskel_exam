module Fall where


import Base
import Rotation

--NEED--
avalanche :: Field -> Field 
avalanche field = searcher field (1, 1)

searcher:: Field -> Key -> Field
searcher field@(t, (n, m)) (a, b) | (isHole (a, b) field) && (a <= n) && (b <= m) && ((a, b) /= (n, m)) = searcher (fall (a, b) field) (nextKey (a, b) n m)
                                  | (isHole (a, b) field) = fall (a, b) field
                                  | (not (isHole (a, b) field)) && (a <= n) && (b <= m) && ((a, b) /= (n, m)) = searcher field (nextKey (a, b) n m)
                                  | otherwise = field

nextKey :: Key -> Int -> Int -> Key
nextKey (a, b) n m | (a <= n) && (b < m) = (a, b + 1)
                   | (a < n) && (b == m) = (a + 1, 1)
                   | otherwise = (0, 0)

isHole :: Key -> Field -> Bool
isHole key field@(t, (n, m)) =  (getValByKey key t) == (0)


fall :: Key -> Field -> Field
fall (a, b) field@(t, (n, m)) | a > 1 = fall (a - 1, b) (swapCells (a, b) (a - 1, b) field)
                              | otherwise = field
