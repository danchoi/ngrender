{-# LANGUAGE Arrows #-}
module Ng.Templating where
import Text.XML.HXT.Core
import Control.Arrow.ArrowList
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs



xs = ["one", "two", "three"]

processTemplate file = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>>
      processTopDown (
        (repeatFragment >>> renderContext "test")
        `when`
        (isElem >>> hasAttr "ng-repeat")
      )
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"

    )

repeatFragment = arrL (take 2 . repeat)


-- renderContext :: ArrowXml a => String -> a b c
renderContext context = processTopDown (
    replaceChildren (imgElement)
    `when`
    (isElem >>> hasName "input")
  )

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
