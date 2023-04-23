module JsonFuntor where

-- import Control.Applicative (Alternative (empty, (<|>)), Applicative (pure, (<*>)))
import Data.Char (isDigit, isSpace, toUpper)
import Data.List
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
divString xn = divString' xn [] 0

parseString :: Parser JsonValue
parseString = Parser divString

divNumber' :: String -> Maybe (JsonValue, String)
divNumber' [] = Nothing
divNumber' xn =
  let (num, str) = span isDigit xn
   in Just (JNumber (read num), str)

divNumber :: String -> Maybe (JsonValue, String)
divNumber [] = Nothing
divNumber (x : xs)
  | isDigit x = divNumber' (x : xs)
  | otherwise = divNumber (xs)

parseNumber :: Parser JsonValue
parseNumber = Parser divNumber

exampleBool :: Maybe (JsonValue, String)
exampleBool = parse parseBool "true hello"

trim :: String -> String
trim = filter (not . isSpace)

removePairs :: String -> Maybe String
removePairs [] = Nothing
removePairs (_ : xs) = Just (init xs)

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
parseTo xs
  | isInfixOf "\"" xs = parseParser xs parseString
  | all isDigit xs = parseParser xs parseNumber
  | isInfixOf "{" xs = parseParser xs parseJson
  | otherwise = parseParser xs parseBool

createKeyValue :: String -> (String, Maybe JsonValue)
createKeyValue xs = let (x, y) = break (== ':') xs in (x, parseTo (tail y))

divJson :: String -> Maybe (JsonValue, String)
divJson [] = Nothing
-- divJson xs = Just ("", JsonObject [("", JBool True)])
divJson xs = Just (JObject (map createKeyValue (splitAcc ',' (removePairs (trim xs)))), "")

parseJson :: Parser JsonValue
parseJson = Parser divJson

exampleJson :: String
exampleJson = "{ \"squadName\": \"Super hero squad\", \"homeTown\": \"Metro City\", \"formed\": 2016, \"secretBase\": \"Super tower\",\"active\": true, hello: {level: 123}}"
