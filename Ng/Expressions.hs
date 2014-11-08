{-# LANGUAGE OverloadedStrings, QuasiQuotes, ExistentialQuantification #-}
module Ng.Expressions where
import Text.Parsec hiding (many, (<|>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Data.List.Split
import Data.Scientific 
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative 
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B
import Test.HUnit 
import Data.String.QQ
import Data.Functor.Identity (Identity )

-- TODO handle filters someone, maybe with externally supplied shell program
-- e.g. note.title | truncate:100

data NgExpr = NgKeyPath [JSKey]
            | Or NgExpr NgExpr
            | And NgExpr NgExpr
            | Neg NgExpr 
            | Compare String NgExpr NgExpr
            | NgLiteral Value
      deriving (Show, Eq)

data JSKey = ObjectKey Text | ArrayIndex Int  | Method Text
    deriving (Show, Eq)

data TextChunk = PassThrough String | Interpolation String 
    deriving (Show, Eq)

data ComparableValue = ComparableNumberValue Scientific
                     | ComparableStringValue Text
                     | ComparableBoolValue Bool
    deriving (Ord, Eq, Show)

symbol s = spaces *> string s <* spaces

ngExpr = do
    maybeNeg <- optionMaybe (symbol "!") 
    expr1' <- ngExprTerm
    let expr1 = case maybeNeg of 
                    Just "!" -> Neg expr1'
                    _ -> expr1'
    try (do symbol "&&"; expr2 <- ngExpr; return $ And expr1 expr2) 
     <|> try (do symbol "||"; expr2 <- ngExpr; return $ Or expr1 expr2) 
     <|> try (do op <- comparisonOp; expr2 <- ngExpr; return $ Compare op expr1 expr2) 
     <|> return expr1

comparisonOp = choice $ map (try . symbol) [">=", "<=", "!=", ">", "<", "=="]

ngExprTerm = (char '(' *> ngExpr <* char ')') <|>  ngLiteral <|> ngKeyPath

ngKeyPath = do
    ks <- sepBy1 ((ArrayIndex . read <$> many1 digit <* char ']') <|> (toJSKey . T.pack <$> ngVarName))
                 (char '.' <|> char '[') 
    return $ NgKeyPath ks 

ngVarName = many1 (alphaNum <|> char '$' <|> char '_')

ngLiteral = NgLiteral <$> (ngNumber <|> ngString <|> ngBool)

-- just integers for now
ngNumber = Number . read <$> many1 digit  

-- dumb simple implementation that does not deal with escaping
ngString = String . T.pack <$> ((char '"' *> many (noneOf "\"") <* char '"') <|> (char '\'' *> many (noneOf "'") <* char '\''))

ngBool = Bool <$> ((try (string "true") *> pure True) <|> (try (string "false") *> pure False))

------------------------------------------------------------------------

-- | function to evaluate an ng-expression and a object value context
-- e.g. Value -> "item.name" -> "John"
ngEvalToString :: Value -> String -> String
ngEvalToString context exprString = valToString . ngExprEval (runParse ngExpr exprString) $ context

ngEvalToBool :: Value -> Text -> Bool
ngEvalToBool context exprString =
    let expr = runParse ngExpr (T.unpack exprString)
        val = ngExprEval expr context
    in valueToBool val

valueToBool :: Value -> Bool
valueToBool (String "") = False
valueToBool (Bool False) = False
valueToBool Null = False
valueToBool (Bool True) = True -- not strictly necessary pattern
valueToBool _ = True

evalText :: Value -> TextChunk -> String
evalText v (PassThrough s) = s
evalText v (Interpolation s) = ngEvalToString v s

ngEval :: [Text] -> Value -> Value
ngEval keyPath context = ngExprEval (NgKeyPath $ map toJSKey keyPath) context

ngExprEval :: NgExpr -> Value -> Value
ngExprEval (NgLiteral x) _ = x
ngExprEval (NgKeyPath ks) v = ngEvaluate ks v
ngExprEval (Or x y) v       = 
      let vx = ngExprEval x v 
      in if (valueToBool vx) then vx else (ngExprEval y v)
ngExprEval (And x y) v      = 
      let vx = ngExprEval x v
      in if (valueToBool vx) then (ngExprEval y v) else (Bool False)
ngExprEval (Neg x) v        = 
      let vx = ngExprEval x v
      in case vx of 
          Null  -> Bool True
          Bool False -> Bool True
          String "" -> Bool True   -- ? is this right?
          _ -> Bool False
ngExprEval (Compare op x y) v      = 
      let vx = comparableValue $ ngExprEval x v
          vy = comparableValue $ ngExprEval y v
      in case op of   
            ">" -> Bool $ vx > vy
            "<" -> Bool $ vx < vy
            ">=" -> Bool $ vx >= vy
            "<=" -> Bool $ vx <= vy
            "==" -> Bool $ vx == vy
            "!=" -> Bool $ vx /= vy


comparableValue :: Value -> ComparableValue
comparableValue (Number x) = ComparableNumberValue x
comparableValue (String x) = ComparableStringValue x
comparableValue (Bool x) = ComparableBoolValue x
comparableValue x = error $ "can't make comparable value for " ++ show x


-- evaluates the a JS key path against a Value context to a leaf Value
ngEvaluate :: [JSKey] -> Value -> Value
ngEvaluate [] x@(String _) = x
ngEvaluate [] x@Null = x
ngEvaluate [] x@(Number _) = x
ngEvaluate [] x@(Bool _) = x
ngEvaluate [] x@(Object _) = x
ngEvaluate [] x@(Array _) = x
ngEvaluate ((ObjectKey key):(Method "length"):[]) (Object s) = 
    case (HM.lookup key s) of
        (Just (Array vs)) -> toJSON $ V.length vs
        _ -> Null
ngEvaluate ((ObjectKey key):xs) (Object s) = ngEvaluate xs (HM.lookupDefault Null key s)
ngEvaluate ((ArrayIndex idx):xs) (Array v) = case V.length v > 0 of
          True -> ngEvaluate xs (v V.! idx)
          False -> Null
ngEvaluate _ _ = Null


-- TODO translate [1] expression
-- CHANGE TO PARSEC
toJSKey :: Text -> JSKey
toJSKey "length" = Method "length"
toJSKey x = ObjectKey x

valToString :: Value -> String
valToString (String x) = T.unpack x
valToString Null = ""
valToString (Bool True) = "true"
valToString (Bool False) = "false"
valToString (Number x) = 
    case floatingOrInteger x of
        Left float -> show float
        Right int -> show int
valToString x = debugJSON x

-- parse String to find interpolation expressions

runParse parser inp =
  case Text.Parsec.parse parser "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs

parseText :: String -> [TextChunk]
parseText = runParse (many ngTextChunk) 

parseKeyExpr :: String -> [JSKey]
parseKeyExpr s = 
      let (NgKeyPath ks) = runParse ngKeyPath s
      in ks

ngTextChunk :: ParsecT String () Identity TextChunk
ngTextChunk = interpolationChunk <|> passThroughChunk

interpolationChunk = do
    try (string "{{")
    spaces
    xs <- manyTill anyChar (lookAhead $ try (string "}}"))
    spaces
    string "}}"
    return $ Interpolation xs

passThroughChunk = PassThrough <$> passThrough

passThrough = do
    -- a lead single { char. This is guaranteed not to be part of {{
    t <- optionMaybe (string "{") 
    xs <- many1 (noneOf "{") 
    x <- (eof *> pure "{{") <|> lookAhead (try (string "{{") <|> string "{")
    res <- case x of 
              "{{" -> return []
              "{" -> ('{':) <$> passThrough 
    return $ (fromMaybe "" t) ++ xs ++ res

-- for debugging
debugJSON = B.unpack . encode 

jsonToValue :: B.ByteString -> Value
jsonToValue = fromJust . decode

------------------------------------------------------------------------
-- Tests

runTests = runTestTT tests

testContext1      = jsonToValue  [s|{"item":"apple","another":10}|]
testContext2      = jsonToValue  [s|{"item":{"name":"apple"}}|]
testContext3      = jsonToValue  [s|{"items":[1,2,3]}|]

tests = test [
    "parseKeyExpr"          ~: [ObjectKey "item"]   @=?   parseKeyExpr "item"
  , "ngEvalToString"        ~: "apple"              @=?   ngEvalToString testContext1 "item" 
  , "ngEvalToString2"       ~: "apple"              @=?   ngEvalToString testContext2 "item.name" 
  , "array keypath"         ~: [ObjectKey "items",ArrayIndex 1]       @=?   parseKeyExpr "items[1]" 
  , "array index"           ~: "2"                  @=?   ngEvalToString testContext3 "items[1]" 
  , "length method"         ~: "3"                  @=?   ngEvalToString testContext3 "items.length" 
  , "parse ngexpr 1"        ~: NgKeyPath [ObjectKey "test"]  
                               @=? runParse ngExpr "test"
  , "parse ngexpr 2"        ~: (Or (NgKeyPath [ObjectKey "test"]) (NgKeyPath [ObjectKey "test2"]))
                               @=? runParse ngExpr "test || test2"
  , "parse ngexpr 3"        ~: (Or (NgKeyPath [ObjectKey "test"]) (NgKeyPath [ObjectKey "test2"]))
                               @=? runParse ngExpr "(test || test2)"
  , "parse ngexpr 4"        ~:  
        And (Or (NgKeyPath [ObjectKey "test1"]) (NgKeyPath [ObjectKey "test2"])) (NgKeyPath [ObjectKey "test3"])
        @=? runParse ngExpr "(test1 || test2) && test3"
  , "parse literal"         ~: NgLiteral (Number 2)  @=? runParse ngExpr "2"
  , "parse negation"        ~: Neg (NgKeyPath [ObjectKey "test"]) @=? runParse ngExpr "!test"
  , "parse comparison"      ~: Compare ">" (NgKeyPath [ObjectKey "test"]) (NgKeyPath [ObjectKey "test2"])
                               @=? runParse ngExpr "test > test2"
  , "parse comparison with literal" ~: 
                               Compare "==" (NgKeyPath [ObjectKey "test"]) (NgLiteral (Number 1))
                               @=? runParse ngExpr "test == 1"
  , "disjunction left"      ~: "apple"               @=? ngEvalToString testContext1 "item || another" 
  , "disjunction right"     ~: "10"                  @=? ngEvalToString testContext1 "blah || another" 
  , "disjunction in parens" ~: "apple"               @=? ngEvalToString testContext2 "(item.color || item.name)" 
  , "length"                ~: Number 3                   @=? ngEval ["items","length"] testContext3 
  , "compare length == "   ~: Bool False
                               @=? ngExprEval (runParse ngExpr "items.length == 2") testContext3
  , "compare length == 2"      ~: Bool True
                               @=? ngExprEval (runParse ngExpr "items.length == 3") testContext3
  , "compare length >"      ~: Bool True
                               @=? ngExprEval (runParse ngExpr "items.length > 1") testContext3
  , "compare length >="     ~: Bool True
                               @=? ngExprEval (runParse ngExpr "items.length >= 1") testContext3
  , "compare to string"     ~: "true"
                               @=? ngEvalToString testContext1 "item == 'apple'"
  , "text chunk 1"          ~: Interpolation "test"  @=? runParse interpolationChunk "{{test}}"
  , "text chunk 2"          ~: PassThrough "test"    @=? runParse passThroughChunk "test"
  , "text chunk 3"          ~: PassThrough " test"   @=? runParse ngTextChunk " test"
  , "text chunk 4"          ~: " test"               @=? runParse passThrough " test"
  , "text chunks"           ~: [PassThrough " test ",Interpolation "test2",PassThrough " test"] @=? parseText " test {{test2}} test"
  , "text empty"            ~: [] @=? parseText ""

             ]




