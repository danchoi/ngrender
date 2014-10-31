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
ngIterate x@(Object _) = interpolate $ valueToText . ngEvaluate (toJSKey "name") $ x
ngIterate _ = none

data JSKey = ObjKey Text | ArrIdx Int 
    deriving Show

toJSKey :: Text -> [JSKey]
toJSKey xs = map go . T.splitOn "." $ xs
  where go x = ObjKey x
        -- TODO translate [1] expression

ngEvaluate :: [JSKey] -> Value -> Value
ngEvaluate [] x@(String _) = x
ngEvaluate [] x@Null = x
ngEvaluate [] x@(Number _) = x
ngEvaluate ((ObjKey key):xs) (Object s) = ngEvaluate xs (HM.lookupDefault Null key s)
ngEvaluate ((ArrIdx idx):xs) (Array v)  = ngEvaluate [] $ v V.! idx
ngEvaluate _ _ = Null

valueToText :: Value -> Text
valueToText (String x) = x
valueToText (Number x) = T.pack $ show x
valueToText Null = ""
valueToText x = T.pack . show $ x



-- renderContext :: ArrowXml a => String -> a b c

{-
renderContext context = processTopDown (
    replaceChildren (imgElement)
    `when`
    (isElem >>> hasName "input")
  )
-}

interpolate :: ArrowXml a => Text  -> a XmlTree XmlTree
interpolate context = processTopDown (
    (constA (show context) >>> mkText)
    `when`
    (isText >>> hasText (isInfixOf "{{item.body}}"))
  )


imgElement :: ArrowXml a => a b XmlTree
imgElement = mkelem "img"                     
	  [ sattr "src" "/icons/ref.png"  
	  , sattr "alt" "external ref"
	  ] [] 
  

    {-
    processAttrl (
      changeAttrValue (const "TSET")
      `when`
      hasName "ng-repeat"
    )
    https://hackage.haskell.org/package/hxt-9.3.1.7

    -}
