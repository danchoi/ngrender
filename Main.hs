module Main where
import Ng.Templating
import Ng.Expressions
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Maybe

main = do
    [file] <- getArgs
    if file == "-t"
    then do
      counts <- runTests 
      print counts
      return ()
    else do
      raw <- B.getContents
      let json = (decode raw :: Maybe Value)
      case json of 
        (Just json') -> processTemplate file json' >> return ()
        _ -> error $ "error parsing json: " ++ B.unpack raw
       
