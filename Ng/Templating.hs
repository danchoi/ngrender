{-# LANGUAGE Arrows #-}
module Ng.Templating where
import Text.XML.HXT.Core
import Control.Arrow.ArrowList

xs = ["one", "two", "three"]

processTemplate file = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>>
      processTopDown (
        repeatFragment
        `when`
        (isElem >>> hasAttr "ng-repeat")
      )
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"

    )

repeatFragment = arrL (take 2 . repeat)

    {-
    processAttrl (
      changeAttrValue (const "TSET")
      `when`
      hasName "ng-repeat"
    )
    https://hackage.haskell.org/package/hxt-9.3.1.7

    -}
