{-# LANGUAGE Arrows, QuasiQuotes, OverloadedStrings #-}
module Ng.Templating where
import Data.Maybe (fromJust)
import Text.XML.HXT.Core
import Control.Arrow.ArrowList
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, intercalate)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.String.QQ 
import qualified Data.Vector as V
import Text.Parsec
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Data.Monoid
import Data.List.Split
import Data.Scientific 
import Ng.Expressions

newtype NgDirective a = NgDirective a
    deriving Show

data NgRepeatKeys = NgRepeatKeys Text [Text]
    deriving Show

processTemplate file context = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>> 
    setTraceLevel 0
    >>>
    process context
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )

process :: Value -> IOSArrow XmlTree XmlTree
process context = 
    normalNgProcess context
    >>>
    processTopDown (

      flatten "ng-href" 
      >>> 
      flatten "ng-src" 
    )

normalNgProcess context = processTopDown (
      ngRepeat context `when` hasNgAttr "ng-repeat"
      >>> interpolateValues context 
      >>> ngClass context
      >>> ngShow context 
      >>> ngHide context  
      >>> ngBind context
      >>> ngBindHtml context
      >>> ngBindHtmlUnsafe context
    )

------------------------------------------------------------------------
-- general interpolation of {{ }} in text nodes


interpolateValues :: ArrowXml a => Value -> a XmlTree XmlTree
interpolateValues context = 
      ((changeText (interpolateText context)) `when` isText)
      >>>
      (processAttrl (changeAttrValue (interpolateText context)) `when` isElem)
   
interpolateText context = mconcat .  map (evalText context) .  parseText

ngBindBase :: String -> Value -> IOSArrow XmlTree XmlTree
ngBindBase tag context = 
    (
      --txt $< (getAttrValue tag >>> arr (ngEvalToString context) )
      replaceChildren (
        (getAttrValue tag >>> arr (ngEvalToString context) ) >>> xread
      ) >>> removeAttr tag
    ) `when` hasNgAttr tag

ngBind = ngBindBase "ng-bind"
ngBindHtml = ngBindBase "ng-bind-html"
ngBindHtmlUnsafe = ngBindBase "ng-bind-html-unsafe"

-- ng-bind-html

-- ng-bind-html-unsafe

------------------------------------------------------------------------
-- ngShow
ngShow :: ArrowXml a => Value -> a XmlTree XmlTree
ngShow context = 
    (
      ((\boolVal -> if boolVal then this else none) 
        $< (getAttrValue "ng-show" >>> arr (ngEvalToBool context . T.pack))
      ) >>> removeAttr "ng-show"
    ) `when` hasNgAttr "ng-show"

-- Not DRY. refactor later
ngHide :: ArrowXml a => Value -> a XmlTree XmlTree
ngHide context = 
    (
      ((\boolVal -> if boolVal then none else this) 
        $< (getAttrValue "ng-hide" >>> arr (ngEvalToBool context . T.pack))
      ) >>> removeAttr "ng-hide"
    ) `when` hasNgAttr "ng-hide"

flatten :: ArrowXml a => String -> a XmlTree XmlTree
flatten name = processAttrl 
      (changeAttrName (const (mkName $ replacement name)))
      `when` (isElem >>> hasAttr name)
    where replacement = drop 3

ngClass context = 
    (
      ((\classNames -> 
          (processAttrl (changeAttrValue (const classNames)))
          -- >>> changeAttrName (const $ mkName "class") `when` (isElem >>> hasAttr "ng-class")  
              -- TODO correct way would be to merge data with any existing class attribute
      ) $< (getAttrValue "ng-class" >>> arr (ngEvalToString context))
      ) -- >>> removeAttr "ng-class"
    ) `when` hasNgAttr "ng-class"
     
------------------------------------------------------------------------
-- ngRepeat

ngRepeat :: Value       -- ^ the global context JSON Value
         -> IOSArrow XmlTree XmlTree
ngRepeat context = 
    (ngRepeatContext context $< ngRepeatKeys) 

ngRepeatKeys :: IOSArrow XmlTree NgRepeatKeys
ngRepeatKeys = 
      getAttrValue "ng-repeat" 
      >>> traceValue 2 (show)
      >>> arr parseNgRepeatExpr
  where parseNgRepeatExpr :: String -> NgRepeatKeys
        parseNgRepeatExpr = runParse $ do
          iterVarName <- ngVarName
          spaces >> string "in" >> spaces
          keyPathToArray <- sepBy1 ngVarName (char '.')
          -- TODO deal with any appended ng-filters
          return $ NgRepeatKeys (T.pack iterVarName) (map T.pack keyPathToArray)


ngRepeatContext :: Value -> NgRepeatKeys -> IOSArrow XmlTree XmlTree
ngRepeatContext c@(Object context) nrp@(NgRepeatKeys iterVarName keyPathToArray) = 
    (\iterVar ->
      traceMsg 2 ("* ngRepeatContext with ngRepeatKeys " ++ show nrp) 
      >>>
      removeAttr "ng-repeat" 
      >>>
      let mergedContext = Object $ HM.insert iterVarName iterVar context
      in (

            ( traceMsg 2 ("nested NGREPEAT context: " ++ (debugJSON mergedContext)) 
            >>> normalNgProcess mergedContext
            )

        )
    ) $< (
            traceMsg 2 ("constL getList key " ++ show keyPathToArray ++ " for context " ++ debugJSON c)
            >>>
            constL (getList $ ngEval keyPathToArray c)
            >>> traceValue 2 show
         )
  -- THIS IS THE BUG. k can be a key path
  where getList :: Value -> [Value]
        getList v = 
            case v of
              Array xs -> V.toList xs
              _ -> []
        -- merge iteration object with general context
ngRepeatContext _ _ = none



------------------------------------------------------------------------

{- idea: pattern matching 

processXml :: Value -> NgDirective -> a XmlTree XmlTree
processXml v (NgRepeat x x) 
processXml v (NgBind  x x) 
processXml v (NgBindHtmlUnsafe  x x) 
processXml v Default 

-}


hasNgAttr :: ArrowXml a => String -> a XmlTree XmlTree
hasNgAttr attrName = isElem >>> hasAttr attrName


