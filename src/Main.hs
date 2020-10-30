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


manageOptions :: StdGen -> String -> (IO (Board), StdGen)
manageOptions gen opt = do
                          case opt of "1" -> (input "", gen)
                                      "2" -> (return(buildBoard [[1]]), gen)
                                      "3" -> let (b, gen') = getRandomBoard gen in (return (b), gen')


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
                let (ioB, gen') = manageOptions gen opt
                b <- ioB
                putStr (getMatrixToStr b)
                sols <- return(pipeline b) 
                let numSols = show $ length sols
                putStr ("This board have " ++ numSols ++ " solutions\n")
                displaySols sols
                console gen'

main :: IO ()
main = do gen <- getStdGen
          putStrLn (intro)
          console gen