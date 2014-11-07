module Main where
import Ng.Templating
import Ng.Tests
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Data.Maybe

main = do
    [file] <- getArgs
    raw <- B.getContents
    let json = (decode raw :: Maybe Value)
    case json of 
      (Just json') -> processTemplate file json'
      _ -> error $ "error parsing json: " ++ B.unpack raw
     
