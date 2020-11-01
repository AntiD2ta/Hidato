module Main where

import Game
import Generator
import Structures
import Strings
import System.IO 
import System.Random
import Control.Monad


input :: String -> IO (Board)
input l = do 
            putStr ("\n")
            putStrLn ("Insert symbols separated by spaces")
            putStrLn ("For white cells insert 0") 
            putStrLn ("For cells with numbers insert numbers")
            putStrLn ("Any other character different of a number greater or equal than 0, will not be considered a cell in the Hidato board")
            putStrLn ("You must always complete a rectangule")
            putStr ("Press enter when you finish\n\n")
            putStr ("Example of a board inserted by input:\n\n")
            putStrLn example
            let b = buildBoard $ getStringToTable example
            putStr (getMatrixToStr b)
            getInput l


getInput :: String -> IO (Board)
getInput l = do
               line <- getLine
               if line == "" 
                    then do
                        let t = getStringToTable(l)
                        if null t 
                            then do
                                putStrLn ("Invalid board. Please try again")
                                input ""
                            else do
                                return (buildBoard t)
                    else do
                        getInput $ l ++ line ++ "\n"


manageOptions :: StdGen -> String -> IO (Board)
manageOptions gen opt = do
                          case opt of "1" -> input ""
                                      "2" -> randomBoard gen
                                      _   -> return(buildBoard [[1]])


getMaybeDif :: Maybe Integer -> Integer
getMaybeDif Nothing = 0
getMaybeDif (Just x) = x


randomBoard :: StdGen -> IO (Board)
randomBoard gen = do putStrLn ("Insert one of the following difficulties. The difficulty is related to the number of white cells.")
                     putStrLn ("Or press enter to go with normal difficulty\n")
                     mapM_ putStrLn displayDif
                     dif <- getLine
                     if dif == "" 
                          then do
                              let b = getRandomBoard gen 40 in return (b)
                          else do
                              let dif' = getMaybeDif $ lookup dif difficulty
                              if dif' == 0
                                  then do
                                      putStrLn ("Invalid difficulty. Try again\n")
                                      randomBoard gen
                                  else do
                                      let b = getRandomBoard gen dif' in return (b)


displaySols :: [Board] -> IO ()
displaySols [] = return ()
displaySols (x:xs) = do
                       putStrLn ("Press enter to see a solution. Insert anything else to continue")
                       line <- getLine
                       if line == "" 
                           then do
                               putStr (getMatrixToStr x)
                               displaySols xs
                           else do
                               putStr ("\n")


console :: StdGen -> IO ()
console gen = do 
                putStr please
                mapM_ putStrLn options
                putStr ("\n")
                opt <- getLine
                let ioB = manageOptions gen opt
                b <- ioB
                when (b == buildBoard [[1]]) $ do
                    putStrLn ("Invalid option\n")
                    console gen
                putStr (getMatrixToStr b)
                sols <- return(pipeline b) 
                let numSols = show $ length sols
                putStr ("This board have " ++ numSols ++ " solutions\n")
                displaySols sols
                gen' <- newStdGen
                console gen'

main :: IO ()
main = do gen <- getStdGen
          putStrLn (intro)
          console gen