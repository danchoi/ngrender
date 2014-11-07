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
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import Text.Parsec
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Data.Monoid
import Data.List.Split
import Data.Scientific 

newtype NgDirective a = NgDirective a
    deriving Show

data NgRepeatParameters = NgRepeatParameters String String 
    deriving Show

processTemplate file context = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>> 
    setTraceLevel 2
    >>>
    process context
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )


process :: Value -> IOSArrow XmlTree XmlTree
process context = 
    processTopDownUntil (    
      hasNgAttr "ng-repeat" `guards` ( ngRepeat context)
    )
    >>>
    processTopDown (
      flatten "ng-href" 
      >>> 
      flatten "ng-src" 
    )

------------------------------------------------------------------------
-- general interpolation of {{ }} in text nodes

-- for debugging
debugJSON = B.unpack . encode 

generalNgProcessing context = 
    hasNgAttr "ng-repeat" `guards` ( ngRepeat context)

{-
interpolateValues context >>> ngShow context >>> ngHide context 
-}


interpolateValues :: ArrowXml a => Value -> a XmlTree XmlTree
interpolateValues context = 
      ((changeText (interpolateText context)) `when` isText)
      >>>
      (processAttrl (changeAttrValue (interpolateText context)) `when` isElem)
   
interpolateText context = mconcat .  map (evalText context) .  parseText

------------------------------------------------------------------------
-- ngShow
ngShow :: ArrowXml a => Value -> a XmlTree XmlTree
ngShow context = 
    (
      ((\boolVal -> if boolVal then this else none) 
        $< (getAttrValue "ng-show" >>> arr (ngEvalToBool context))
      ) >>> removeAttr "ng-show"
    ) `when` hasNgAttr "ng-show"

-- Not DRY. refactor later
ngHide :: ArrowXml a => Value -> a XmlTree XmlTree
ngHide context = 
    (
      ((\boolVal -> if boolVal then none else this) 
        $< (getAttrValue "ng-hide" >>> arr (ngEvalToBool context))
      ) >>> removeAttr "ng-hide"
    ) `when` hasNgAttr "ng-hide"

------------------------------------------------------------------------

flatten :: ArrowXml a => String -> a XmlTree XmlTree
flatten name = processAttrl 
      (changeAttrName (const (mkName $ replacement name)))
      `when` (isElem >>> hasAttr name)
    where replacement = drop 3

------------------------------------------------------------------------
-- ngRepeat

ngRepeat :: Value       -- ^ the global context JSON Value
         -> IOSArrow XmlTree XmlTree
ngRepeat context = 
    (ngRepeatContext context $< ngRepeatKeys) 

ngRepeatKeys :: IOSArrow XmlTree NgRepeatParameters
ngRepeatKeys = 
      getAttrValue "ng-repeat" 
      >>> traceValue 2 (show)
      >>> arr parseNgRepeatExpr
  where parseNgRepeatExpr :: String -> NgRepeatParameters
        parseNgRepeatExpr = runParse $ do
          iter <- ngVarName
          spaces >> string "in" >> spaces
          context <- ngVarName
          -- TODO deal with any appended ng-filters
          return $ NgRepeatParameters iter context

ngVarName = many1 (alphaNum <|> char '$' <|> char '_')

ngRepeatContext :: Value -> NgRepeatParameters -> IOSArrow XmlTree XmlTree
ngRepeatContext (Object context) nrp@(NgRepeatParameters iterKey contextKey) = 
    (\iterVar ->
      traceMsg 2 ("ngRepeatContext with keys " ++ show nrp) 
      >>>
      removeAttr "ng-repeat" 
      >>>
      let mergedContext = Object $ HM.insert (T.pack iterKey) iterVar context
      in (
          processTopDown (

              ( traceMsg 2 ("nested NGREPEAT context: " ++ (debugJSON mergedContext)) 
              >>> ngRepeat mergedContext `when` hasNgAttr "ng-repeat"
              )



          )
        )
    ) $< (constL $ getList (T.pack contextKey) context)
  where getList :: Text -> HM.HashMap Text Value -> [Value]
        getList k v = 
            case HM.lookup k v of
              Just (Array xs) -> V.toList xs
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


evalText :: Value -> TextChunk -> String
evalText v (PassThrough s) = s
evalText v (Interpolation s) = ngEvalToString v s 


-- | function to evaluate an ng-expression and a object value context
-- e.g. "item.name" -> (Object ...) -> "John"

ngEvalToString :: Value -> String -> String
ngEvalToString context keyExpr = valToString . ngEvaluate (parseKeyExpr keyExpr) $ context

ngEvalToBool :: Value -> String -> Bool
ngEvalToBool context keyExpr =
    let keys = toJSKey keyExpr
        val = ngEvaluate keys context
    in valueToBool val

valueToBool :: Value -> Bool
valueToBool (String "") = False
valueToBool (Bool False) = False
valueToBool Null = False
valueToBool (Bool True) = True -- not strictly necessary pattern
valueToBool _ = True

data JSKey = ObjectKey String | ArrayIndex Int 
    deriving Show

-- evaluates the a JS key path against a Value context to a leaf Value
ngEvaluate :: [JSKey] -> Value -> Value
ngEvaluate [] x@(String _) = x
ngEvaluate [] x@Null = x
ngEvaluate [] x@(Number _) = x
ngEvaluate [] x@(Bool _) = x
ngEvaluate [] x@(Object _) = x
ngEvaluate [] x@(Array _) = x
ngEvaluate ((ObjectKey key):xs) (Object s) = ngEvaluate xs (HM.lookupDefault Null (T.pack key) s)
ngEvaluate ((ArrayIndex idx):xs) (Array v)  = ngEvaluate [] $ v V.! idx
ngEvaluate _ _ = Null

-- TODOO key may have ngFILTER appended. Just ignore it.
-- Move this to the parse
toJSKey :: String -> [JSKey]
toJSKey xs = map go . splitOn "." $ xs
  where go x = ObjectKey x
        -- TODO translate [1] expression

valToString :: Value -> String
valToString (String x) = T.unpack x
valToString Null = ""
valToString (Bool True) = "true"
valToString (Bool False) = "false"
valToString (Number x) = 
    case floatingOrInteger x of
        Left float -> show float
        Right int -> show int
valToString x = show  x

-- parse String to find interpolation expressions

data TextChunk = PassThrough String | Interpolation String 
    deriving Show

runParse parser inp =
  case Text.Parsec.parse parser "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs

parseText :: String -> [TextChunk]
parseText = runParse (many ngTextChunk) 

parseKeyExpr :: String -> [JSKey]
parseKeyExpr = runParse ngKey

-- ignores ngFilters after keys
-- e.g. note.title | truncate:100
-- TODO handle filters someone, maybe with externally supplied shell program
ngKey = do
    ks <- sepBy1 ngVarName (char '.') 
    return $ toJSKey (intercalate "." ks)

    

-- ngTextChunk :: Stream s m Char => ParsecT s u m TextChunk
ngTextChunk =   
    (Interpolation <$> (string "{{" *> many1 (noneOf "}") <* string "}}"))
    <|> (PassThrough <$> (many1 (noneOf "{")))


hasNgAttr :: ArrowXml a => String -> a XmlTree XmlTree
hasNgAttr attrName = isElem >>> hasAttr attrName


