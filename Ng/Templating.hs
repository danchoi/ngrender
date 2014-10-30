{-# LANGUAGE Arrows, QuasiQuotes, OverloadedStrings #-}
module Ng.Templating where
import Text.XML.HXT.Core
import Control.Arrow.ArrowList
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs
import Data.List
import Data.Aeson
import Data.Aeson.Types
import Data.String.QQ 
import qualified Data.ByteString.Lazy.Char8 as B


xs = ["one", "two", "three"]

items :: Maybe Value
items = decode $ B.pack [s|[{"name":"one"}, {"name":"two"},{"name":"three"}]|]


processTemplate file = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>>
      processTopDown (
        -- (repeatFragment >>> renderContext "test")
        (repeatFragment >>> (interpolate "test"))
        `when`
        (isElem >>> hasAttr "ng-repeat")
      )
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"

    )

repeatFragment = arrL (take 2 . repeat)


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
