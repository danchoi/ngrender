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
import Control.Applicative ((<*), (*>), (<$>))
import Data.Monoid
import Data.List.Split

items :: Value
items = fromJust $ decode $ B.pack [s|[{"name":"one","votes":1}, {"name":"two","votes":2},{"name":"three","votes":3}]|]

context :: Value
context = Object (HM.singleton "items" items)

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
    (ngIterate $< (constL $ map (Object . HM.singleton "item") . V.toList $ xs))
    >>> removeAttr "ng-repeat" 
ngRepeat _ = this

ngIterate :: ArrowXml a => Value -> a XmlTree XmlTree
ngIterate iterVar@(Object _) = 
        processTopDown (
          (changeText (mconcat . map (evalText iterVar) . parseText))
          `when`
          isText 
        )
ngIterate _ = none


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

parseText :: String -> [TextChunk]
parseText inp = 
  case Text.Parsec.parse (many ngTextChunk) "" inp of
    Left x -> error $ "parseText failed: " ++ show x
    Right xs -> xs
    

-- ngTextChunk :: Stream s m Char => ParsecT s u m TextChunk
ngTextChunk =   
    (Interpolation <$> (string "{{" *> many1 (noneOf "}") <* string "}}"))
    <|> (PassThrough <$> (many1 (noneOf "{")))

