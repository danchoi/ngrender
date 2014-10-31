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
import Text.Parsec
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Data.Monoid
import Data.List.Split



processTemplate file json = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>>
      processTopDown (
        ngRepeat json
        `when`
        (isElem >>> hasAttr "ng-repeat")
      )
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )

ngRepeat :: ArrowXml a 
         => Value       -- ^ the global context JSON Value, the local loop JSON Value 
         -> a XmlTree XmlTree
ngRepeat context = 
    (ngIterate context $< ngRepeatKeys)
    >>> removeAttr "ng-repeat" 


ngRepeatKeys :: ArrowXml a => a XmlTree NgRepeat
ngRepeatKeys = getAttrValue "ng-repeat" >>> arr parseNgRepeatExpr

ngIterate :: ArrowXml a 
          => Value      -- ^ context
          -> NgRepeat   -- ^ contains data to repeat
          -> a XmlTree XmlTree
ngIterate (Object context) (NgRepeat iterKey contextKey) = 
    go iterKey $< (constL $ getList (T.pack contextKey) context)
  where getList :: Text -> HM.HashMap Text Value -> [Value]
        getList k v = 
            case HM.lookup k v of
              Just (Array xs) -> V.toList xs
              _ -> []
        go iterKey iterVar = 
          processTopDown (
            (changeText (
                mconcat . 
                map (evalText (wrapObjInKey (T.pack iterKey) iterVar)) . 
                parseText
              )
            )
            `when`
            isText 
          )
ngIterate _ _ = none

wrapObjInKey :: Text -> Value -> Value
wrapObjInKey k v = Object $ HM.singleton k v


evalText :: Value -> TextChunk -> String
evalText v (PassThrough s) = s
evalText v (Interpolation s) = ngEval s v


-- | function to evaluate an ng-expression and a object value context
-- e.g. "item.name" -> (Object ...) -> "John"
ngEval :: String -> Value -> String
ngEval keyExpr context = valToString . ngEvaluate (toJSKey keyExpr) $ context

data JSKey = ObjectKey String | ArrayIndex Int 
    deriving Show

ngEvaluate :: [JSKey] -> Value -> Value
ngEvaluate [] x@(String _) = x
ngEvaluate [] x@Null = x
ngEvaluate [] x@(Number _) = x
ngEvaluate ((ObjectKey key):xs) (Object s) = ngEvaluate xs (HM.lookupDefault Null (T.pack key) s)
ngEvaluate ((ArrayIndex idx):xs) (Array v)  = ngEvaluate [] $ v V.! idx
ngEvaluate _ _ = Null

toJSKey :: String -> [JSKey]
toJSKey xs = map go . splitOn "." $ xs
  where go x = ObjectKey x
        -- TODO translate [1] expression

valToString :: Value -> String
valToString (String x) = T.unpack x
valToString (Number x) = show x
valToString Null = ""
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

    

-- ngTextChunk :: Stream s m Char => ParsecT s u m TextChunk
ngTextChunk =   
    (Interpolation <$> (string "{{" *> many1 (noneOf "}") <* string "}}"))
    <|> (PassThrough <$> (many1 (noneOf "{")))

-- | repeats the iteration placeholder key and the key tot he global context object
data NgRepeat = NgRepeat String String  deriving Show

parseNgRepeatExpr :: String -> NgRepeat 
parseNgRepeatExpr = runParse $ do
        iter <- many1 alphaNum
        spaces >> string "in" >> spaces
        context <- many1 alphaNum
        return $ NgRepeat iter context


