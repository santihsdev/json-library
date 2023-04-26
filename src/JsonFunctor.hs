module JsonFunctor
  ( getJLine,
    exampleJson,
    exampleJson2,
    parse,
    parseBool,
    parseString,
    parseNumber,
    parseArray,
    parseJson
  )
where

-- import Control.Applicative (Alternative (empty, (<|>)), Applicative (pure, (<*>)))

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Char (isDigit, isSpace)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import JsonObject (JsonValue (..))

data Parser a = Parser {parse :: String -> Maybe (a, String)}

instance Functor Parser where
  -- fmap f a = pure f <*> a
  fmap f a = f <$> a

instance Applicative Parser where
  pure f = Parser (\p -> Just (f, p))
  (<*>) (Parser f) (Parser b) =
    Parser
      ( \x -> do
          (f', _) <- f x
          (b', res) <- b x
          Just (f' b', res)
      )

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  (<|>) (Parser f) (Parser b) = Parser (\x -> f x <|> b x)

parseBool :: Parser JsonValue
parseBool =
  Parser
    ( \x -> case x of
        't' : 'r' : 'u' : 'e' : xs -> Just (JBool True, xs)
        'f' : 'a' : 'l' : 's' : 'e' : xs -> Just (JBool False, xs)
        _ -> Nothing
    )

divString' :: String -> String -> Int -> Maybe (JsonValue, String)
divString' [] res _
  | not (null res) = Just (JString "", reverse res)
  | otherwise = Nothing
divString' (x : xs) res counter
  | x == '\"' && counter == 1 = Just (JString (reverse res), xs)
  | x == '\"' = divString' xs res (counter + 1)
  | counter == 1 = divString' xs (x : res) counter
  | otherwise = divString' xs res counter

divString :: String -> Maybe (JsonValue, String)
divString (x : _)
  | x == '[' || x == '{' = Nothing
divString xn = divString' xn [] 0

parseString :: Parser JsonValue
parseString = Parser divString

divNumber :: String -> Maybe (JsonValue, String)
divNumber [] = Nothing
divNumber (x : _)
  | x == '[' || x == '{' = Nothing
divNumber xn@(x : xs)
  | all isDigit xs && x == '-' =
      let (num, str) = span isDigit xs
       in Just (JNumber (-1 * read num), str)
  | all isDigit xn =
      let (num, str) = span isDigit xn
       in Just (JNumber (read num), str)
  | otherwise = Nothing

parseNumber :: Parser JsonValue
parseNumber = Parser divNumber

trim :: String -> String
trim = filter (not . isSpace)

removePairs :: String -> Char -> Char -> Maybe String
removePairs [] _ _ = Nothing
removePairs (x : xs) b e
  | x == b && last xs == e = Just (init xs)
  | otherwise = Nothing

splitAcc :: Char -> Maybe String -> [String]
splitAcc _ Nothing = []
splitAcc c (Just xs) = splitAcc' c xs [] 0 0

splitAcc' :: Char -> String -> String -> Int -> Int -> [String]
splitAcc' _ [] [] _ _ = []
splitAcc' _ [] ys _ _ = [reverse ys]
splitAcc' c (x : xs) ys bracket brace
  | x == c && x == ',' && (bracket == 0 && brace == 0) = reverse ys : splitAcc' c xs [] bracket brace
  | x == c && x == ':' && (brace == 0 && bracket == 0) = reverse ys : splitAcc' c xs [] bracket brace
  | x == '[' = splitAcc' c xs (x : ys) (bracket + 1) brace
  | x == '{' = splitAcc' c xs (x : ys) bracket (brace + 1)
  | x == ']' = splitAcc' c xs (x : ys) (bracket - 1) brace
  | x == '}' = splitAcc' c xs (x : ys) bracket (brace - 1)
  | otherwise = splitAcc' c xs (x : ys) bracket brace

parseParser :: String -> Parser JsonValue -> Maybe JsonValue
parseParser xs p =
  let pos = parse p xs
   in case pos of
        Nothing -> Nothing
        Just (res, _) -> Just res

parseTo :: String -> Maybe JsonValue
parseTo [] = Nothing
parseTo xs@(x : xn)
  | x == '[' = parseParser xs parseArray
  | x == '{' = parseParser xs parseJson
  | "\"" `isInfixOf` xs = parseParser xs parseString
  | all isDigit xn = parseParser xs parseNumber
  | otherwise = parseParser xs parseBool

createKeyValue :: String -> (String, Maybe JsonValue)
createKeyValue xs = let (x, y) = break (== ':') xs in (x, parseTo (tail y))

divJson :: String -> Maybe (JsonValue, String)
divJson [] = Nothing
divJson (x : xs)
  | x == '[' || last xs == ']' = Nothing
-- divJson xs = Just ("", JsonObject [("", JBool True)])
divJson xs = Just (JObject (map createKeyValue (splitAcc ',' (removePairs (trim xs) '{' '}'))), "")

parseJson :: Parser JsonValue
parseJson = Parser divJson

extractJust :: [Maybe JsonValue] -> [JsonValue]
extractJust [] = []
extractJust (x : xs) = case x of
  Nothing -> extractJust xs
  (Just s) -> s : extractJust xs

divArray :: String -> Maybe (JsonValue, String)
divArray [] = Nothing
divArray (x : xs)
  | x /= '[' || last xs /= ']' = error "List not valid"
divArray xs = Just (JList (extractJust (map parseTo (splitAcc ',' (removePairs (trim xs) '[' ']')))), "")

parseArray :: Parser JsonValue
parseArray = Parser divArray

parseNil :: Parser JsonValue
parseNil =
  Parser
    ( \x -> case x of
        "" -> Just (JNil, "")
        _ -> Nothing
    )

exampleJson :: String
exampleJson = "{ \"squadName\": \"Super hero squad\", \"homeTown\": \"Metro City\", \"formed\": 2016, \"secretBase\": \"Super tower\",\"active\": true, hello: {level: -123}, array: [{hello: [123]}, \"world\", 123]}"

exampleJson2 :: String
exampleJson2 = "{\"squadName\": \"Super hero squad\",\"homeTown\": \"Metro City\",\"formed\": 2016,\"secretBase\": \"Super tower\",\"active\": true, \"members\": [   {     \"name\": \"Molecule Man\",     \"age\": 29,     \"secretIdentity\": \"Dan Jukes\",     \"powers\": [\"Radiation resistance\", \"Turning tiny\", \"Radiation blast\"]   },   {     \"name\": \"Madame Uppercut\",     \"age\": 39,     \"secretIdentity\": \"Jane Wilson\",     \"powers\": [       \"Million tonne punch\",       \"Damage resistance\",       \"Superhuman reflexes\"     ]   },   {     \"name\": \"Eternal Flame\",     \"age\": 1000000,     \"secretIdentity\": \"Unknown\",     \"powers\": [       \"Immortality\",       \"Heat Immunity\",       \"Inferno\",       \"Teleportation\",       \"Interdimensional travel\"]}]}"

getJLine :: String -> JsonValue
getJLine xs = fst (fromJust (parse (parseString <|> parseBool <|> parseNumber <|> parseJson <|> parseArray <|> parseNil) xs))

modify :: JsonValue -> JsonValue
modify (JObject s) = JObject [(x, Just (modify (fromJust y))) | (x, y) <- s]
modify (JNumber n) = JNumber (n + 10)
modify (JString s) = JString (s ++ "hello")
modify (JBool b) = JBool (not b)
modify (JList l) = JList (fmap modify l)
modify d = d

appTest :: Maybe JsonValue
appTest = (\(j, _) -> modify j) <$> parse parseJson exampleJson
