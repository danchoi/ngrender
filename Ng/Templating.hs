{-# LANGUAGE Arrows, QuasiQuotes, OverloadedStrings #-}
module Ng.Templating where
import Data.Maybe (fromJust)
import Text.XML.HXT.Core
import Control.Arrow.ArrowList
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.String.QQ 
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import Text.Regex

items :: Value
items = fromJust $ decode $ B.pack [s|[{"name":"one"}, {"name":"two"},{"name":"three"}]|]

context :: Value
context = Object (HM.singleton "items" items)

-- Array (fromList [Object (fromList [("name",String "one")]),Object (fromList [("name",String "two")]),Object (fromList [("name",String "three")])])

processTemplate file = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>>
      processTopDown (
        ngRepeat (items, items) 
        `when`
        (isElem >>> hasAttr "ng-repeat")
      )
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )

ngRepeat :: ArrowXml a => (Value, Value) -> a XmlTree XmlTree
ngRepeat (globalContext, loop@(Array xs)) = 
    (ngIterate $< (constL $ V.toList xs))
    >>> removeAttr "ng-repeat" 
ngRepeat _ = this

ngIterate :: ArrowXml a => Value -> a XmlTree XmlTree
ngIterate x@(Object _) = interpolate $ ngEval "name" x
ngIterate _ = none

-- CHANGE: replace strings

interpolate :: ArrowXml a => String -> a XmlTree XmlTree
interpolate replace = processTopDown (
    (changeText (gsub "{{item.body}}" replace))
    `when`
    (isText >>> hasText (isInfixOf "{{item.body}}"))
  )

-- | function to evaluate an ng-expression and a object value context
-- e.g. "item.name" -> (Object ...) -> "John"
ngEval :: Text -> Value -> String
ngEval keyExpr context = valueToText . ngEvaluate (toJSKey keyExpr) $ context

data JSKey = ObjectKey Text | ArrayIndex Int 
    deriving Show


ngEvaluate :: [JSKey] -> Value -> Value
ngEvaluate [] x@(String _) = x
ngEvaluate [] x@Null = x
ngEvaluate [] x@(Number _) = x
ngEvaluate ((ObjectKey key):xs) (Object s) = ngEvaluate xs (HM.lookupDefault Null key s)
ngEvaluate ((ArrayIndex idx):xs) (Array v)  = ngEvaluate [] $ v V.! idx
ngEvaluate _ _ = Null

toJSKey :: Text -> [JSKey]
toJSKey xs = map go . T.splitOn "." $ xs
  where go x = ObjectKey x
        -- TODO translate [1] expression

valueToText :: Value -> String
valueToText (String x) = T.unpack x
valueToText (Number x) = show x
valueToText Null = ""
valueToText x = show  x



-- | Behaves like Ruby gsub implementation
-- adapted from: https://coderwall.com/p/l1hoeq

gsub :: String -> String -> String -> String
gsub regex replace str =
  case matchRegexAll (mkRegex regex) str of
    Nothing -> str
    Just (before, matched, after, _) ->
      before ++ replace ++ (gsub regex replace after)
