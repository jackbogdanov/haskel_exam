module Main where

import Fall
import Check 
import Print
import Rotation
import Base
import Text.Parsec
import System.Random
import Control.Monad

-- cont >>= return == cont
-- return m >>= f == f m
-- cont >>= f >>= g == cont >>= (\x -> f x >>= g)

main :: IO ()
main = do
    putStr "Enter field size: "
    input <- getLine
    case parse (sepBy (many1 digit) (char ' ')) "" input of
        Left err  -> do {putStrLn "Wrong!"; main}  -- main >>= (\_ -> putStrLn "Wrong!") |  m a -> (a -> m b) -> m b 
        Right [n, m]  -> do
               rand <- makeRanGood $ n' * m'
               field <- return (createField n' m' rand)
               printField $ field
               (field', score) <- crashLoop field (isCrashFinished field) 0
               --printField $ field'
               putStrLn ("SCORE: " ++ show score)
               startMainGame field' score
                               where
                  n' = read n
                  m' = read m
        Right x -> do {putStrLn "Wrong!"; main}


startMainGame :: Field -> Score -> IO ()
startMainGame field score = do 
    putStr "Enter coord of triple to rotate: "
    input <- getLine
    case input of
      "Exit" -> do {putStrLn ("Ok, Game over. Your result = " ++ show(score))}
      otherwise -> do 
        case parse (sepBy (between (char '(') (char ')') (sepBy (many1 digit) (char ','))) (char ' ')) "" input of
          Left err  -> do {putStrLn "Wrong input!"; startMainGame field score}
          Right [[i1,j1],[i2,j2],[i3,j3]] -> do 
                       if not $ userRotationCheck k1 k2 k3 field then do {putStrLn "Incorrect coords!!"; startMainGame field score} else do
                          putStr "Enter rotation kind: "
                          input' <- getLine
                          case input' of
                           "1" -> do
                              field' <- return $ rotateCells1 k1 k2 k3 field
                              (field'', score') <- crashLoop field' (isCrashFinished field') score
                              --printField $ field''
                              putStrLn ("SCORE: " ++ show score')
                              startMainGame field'' score'
                           "2" -> do
                              field' <- return $ rotateCells2 k1 k2 k3 field
                              (field'', score') <- crashLoop field' (isCrashFinished field') score
                              --printField $ field''
                              putStrLn ("SCORE: " ++ show score')
                              startMainGame field'' score'
                           otherwise -> do {putStrLn "Incorrect rotate!!"; startMainGame field score}
                         where
                              k1 = (read i1, read j1)
                              k2 = (read i2, read j2)
                              k3 = (read i3, read j3)
          Right x -> do {putStrLn "Wrong!"; startMainGame field score}

makeRanGood :: Int -> IO [Int]
makeRanGood n = sequence $ makeRandom n

makeRandom :: Int -> [IO Int]
makeRandom 0 = []
makeRandom n = (randomRIO (1, 9)) : makeRandom (n - 1)

crashLoop :: Field -> Bool -> Score -> IO ((Field, Score))
crashLoop fld True score = return (fld, score)
crashLoop fld@(tb,(n,m)) False score = do
     (fld', score') <- return $ removeWinParts fld score
     printField $ fld'
     fld'' <- return $ avalanche fld'
     printField $ fld''
     rand <- makeRanGood $ n * m
     fld''' <- return $ addNewNodes rand fld'' 
     printField $ fld'''
     crashLoop fld''' (isCrashFinished fld''') score'


--workingFun :: 
--inputCoordsParser :: Parsec s () a
--inputCoordsParser = sepBy (between (char '(') (char ')') (sepBy (many1 digit) (char ','))) (char ' ')

-- sepBy ((,) <$> many1 digit <*> many1 digit) (char ' ')
-- parseTest ((,) <$> many1 digit <*> skipMany1 (char ' ') *> many1 digit)  "1 1"
--parseTest (between (char '(') (char ')') (many1 digit)) "(123)"
-- parseTest (between (char '(') (char ')') (sepBy (many1 digit) (char ','))) "(12,3)"

-- parseTest (sepBy (between (char '(') (char ')') (sepBy (many1 digit) (char ','))) (char ' '))










