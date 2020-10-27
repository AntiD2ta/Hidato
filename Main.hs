import Game
import Structures
import System.IO 

main = do b <- return(pipeline)
          sequence $ map (\b' -> putStr (getMatrixToStr b')) b
          print(length b)
          --putStr (getMatrixToStr b)