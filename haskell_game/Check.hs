module Check where 


import Base

userRotationCheck :: Key -> Key -> Key -> Field -> Bool
userRotationCheck k1@(i1, j1) k2@(i2, j2) k3@(i3, j3) fld@(tb,(height, width)) = 
                                         null lst1 && null lst2 
                                         && k1 /= k2 && k2 /= k3 && k3 /= k1 
                                         && isInField k1 && isInField k2 && isInField k3 && triangleCheck k1 k2 k3 where
                                                   isInField (a,b) = a >= 1 && a < height && b >= 1 && b < width
                                                   lst1 = [(i, j) | i <- [i1, i2, i3], j <- [i1, i2, i3], (abs $ i - j) > 1]
                                                   lst2 = [(i, j) | i <- [j1, j2, j3], j <- [j1, j2, j3], (abs $ i - j) > 1]

triangleCheck :: Key -> Key -> Key -> Bool
triangleCheck k1@(i1, j1) k2@(i2, j2) k3@(i3, j3) = case getLineCells k1 k2 k3 of
                                                        Nothing -> False
                                                        Just (k1'@(i1, j1), k2'@(i2, j2)) -> if odd i1 then check (min j1 j2) [k| k <- [k1, k2, k3], k /= k1' && k /= k2'] else check (max j1 j2) [k| k <- [k1, k2, k3], k /= k1' && k /= k2']


check :: Int -> [Key] -> Bool
check j [(i3,j3)] = (j == j3)
check _ _ = False


getLineCells :: Key -> Key -> Key -> Maybe (Key, Key)
getLineCells k1 k2 k3 = if null lst then Nothing else Just $ head lst where
                             lst = [(a, b)| a <- [k1, k2, k3], b <- [k1, k2, k3], a /= b, (fst a) == (fst b)]