module Print (printField) where

import Base

printUp :: Int -> IO ()
printUp 1 = do
    putStrLn "  _ "
printUp n = do
    putStr "  _ "
    printUp (n - 1)

printUpSlesh :: Int -> IO ()
printUpSlesh 1 = do
    putStrLn " / \\"
printUpSlesh n = do
    putStr " / \\"
    printUpSlesh (n - 1)


printStrOfField :: Int -> Int -> Table -> IO ()
printStrOfField i = printStrOfField' i 1


printStrOfField' :: Int -> Int -> Int -> Table -> IO ()
printStrOfField' i j n tb | j == (n + 1) = putStrLn "|"
                          | otherwise = do
                            putStr ("| " ++ show (getValByKey (i, j) tb) ++ " ")
                            printStrOfField' i (j + 1) n tb



printDown :: Int -> IO ()
printDown 1 = do
    putStrLn " \\_/"
printDown n = do
    putStr " \\_/"
    printDown (n - 1)



printField :: Field -> IO ()
printField field@(tb, size) = do
    printUp n
    printUpSlesh n
    printField' 1 field  where  n = snd size


printField' :: Int -> Field -> IO ()
printField' i field@(tb, (n, m)) | i == (n + 1) = putStr "" 
                                 | otherwise =  do
                                   printStrOfField i m tb
                                   printDown m
                                   putStr "  "
                                   if (i == n) then putStr "" else do
                                        printStrOfField (i + 1) m tb
                                        putStr " /"
                                        printDown m
                                        printField' (i + 2) field

                                   


-- printField (createField 10 10 [9, 9..])
-- printField (deleteCell (4,4) (createField 10 10 [9,9..]))





