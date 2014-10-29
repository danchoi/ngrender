module Main where
import Ng.Templating
import System.Environment

main = do
    [file] <- getArgs
    processTemplate file
     
