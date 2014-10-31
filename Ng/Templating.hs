{-# LANGUAGE Arrows, QuasiQuotes, OverloadedStrings #-}
module Ng.Templating where
import Data.Maybe (fromJust)
import Text.XML.HXT.Core
import Control.Arrow.ArrowList
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs
import Data.List
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.String.QQ 
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V


xs = ["one", "two", "three"]

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
    -- arrL (take (V.length xs) . repeat) >>> 
    (this &&& (constL $ V.toList xs))
    >>> arr2 ngIterate
    -- >>> removeAttr "ng-repeat" 

ngRepeat _ = this

ngIterate :: XmlTree -> Value -> XmlTree
ngIterate x v = x


-- renderContext :: ArrowXml a => String -> a b c

{-
renderContext context = processTopDown (
    replaceChildren (imgElement)
    `when`
    (isElem >>> hasName "input")
  )
-}

interpolate :: ArrowXml a => String -> a XmlTree XmlTree
interpolate context = processTopDown (
    (constA context >>> mkText)
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
