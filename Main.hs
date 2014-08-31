module Main where

import Frobnicator

measureTimes :: Context -> Int -> IO ()
measureTimes context n =
  do result <- measure context
     putStrLn $ "HS: Measure result #" ++ (show n) ++ " -- " ++ (show result)
     if n > 0
     then measureTimes context $ n - 1
     else return ()

doMeasurements :: Context -> IO ()
doMeasurements context =
  do putStrLn $ "HS: Obtained initialized context -- ptr = " ++
                (show context)
     measureTimes context 5
     putStrLn "HS: Releasing context"

main :: IO ()
main =
  do putStrLn "HS: Initializing context"
     result <- withContext doMeasurements
     case result of
       Just _  -> putStrLn "HS: Done!"
       Nothing -> putStrLn "HS: Failed to obtain context"
