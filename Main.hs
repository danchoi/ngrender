module Main where
import Ng.Templating
import Ng.Expressions
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Maybe

main = do
    files <- getArgs
    if head files == "-t"
    then do
      counts <- runTests 
      print counts
      return ()
    else do
      raw <- B.getContents
      let json = (decode raw :: Maybe Value)
      case json of 
        (Just json') -> do
            case files of 
                [file] -> processTemplate file json' >> return ()
                [layout, file] -> processTemplateWithLayout layout file json' >> return ()
        _ -> error $ "error parsing json: " ++ B.unpack raw
       
