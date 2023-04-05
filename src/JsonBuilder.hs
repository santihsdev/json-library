module JsonBuilder (writeJson, parseJson, reverseExample) where

import Data.Char (isDigit, isSpace)
import Data.List (delete, intercalate, isPrefixOf)
import Data.Maybe (isJust)
import JsonObject (JsonValue (..))

reverseExample :: String
reverseExample = "{\"favoriteColors\":[[\"hello\",[\"hello2\"]],\"yellow\",\"red\"]}"

example :: JsonValue
example =
  JObject
    [ ("name", Just (JString "Desiree")),
      ("age", Just (JNumber 19.9)),
      ("activities", Just (JList [Just (JString "sleep"), Just (JString "eat"), Just (JString "study")]))
    ]

writeJson :: Maybe JsonValue -> String
writeJson (Just (JString s)) = s
writeJson (Just (JBool b)) = show b
writeJson (Just (JNumber n)) = show n
writeJson (Just (JList l)) = show (map writeJson l)
writeJson (Just (JObject o)) = "{" ++ createFromObject o ++ "}"

showNode :: (String, Maybe JsonValue) -> String
showNode (name, value) = name ++ ": " ++ writeJson value

createFromObject :: [(String, Maybe JsonValue)] -> String
createFromObject [] = ""
createFromObject xs = intercalate ", " (map showNode xs)

containsList :: String -> Char -> Bool
containsList xs _ = isPrefixOf "[" xs && isPrefixOf "]" xs

parseJson :: String -> Maybe JsonValue
parseJson [] = Nothing
parseJson s = Just (JObject (map buildTuple (splitAcc' ',' (extractObject (Just s) '{' '}') [] 0)))

extractObject :: Maybe String -> Char -> Char -> Maybe String
extractObject Nothing _ _ = Nothing
extractObject (Just (_ : xs)) i f = Just (dropWhile (== i) (takeWhile (/= f) xs))

extractObject' :: Maybe String -> Maybe String
extractObject' Nothing = Nothing
extractObject' (Just []) = Nothing
extractObject' (Just (_ : xs)) = Just (init xs)

{-Mapear para que devuelva tuplas y qu sean tupplas de string y jsonvalue-}
parseString :: String -> Maybe JsonValue
parseString "" = Nothing
parseString ('\"' : xs) = Just (JString (takeWhile (/= '\"') xs))
parseString _ = Nothing

parseBool :: String -> Maybe JsonValue
parseBool ('f' : 'a' : 'l' : 's' : 'e' : _) = Just (JBool False)
parseBool ('t' : 'r' : 'u' : 'e' : _) = Just (JBool True)
parseBool _ = Nothing

parseNumber :: String -> Maybe JsonValue
parseNumber n = if isDigit' n then Just (JNumber (read n)) else Nothing

parseList :: String -> Maybe JsonValue
parseList [] = Nothing
parseList xs = Just (JList (parseList' xs))

parseList' :: String -> [Maybe JsonValue]
parseList' [] = [Just JNil]
parseList' xs = if isOpenCharList xs then map getJValue (splitAcc' ',' (extractObject' (Just xs)) [] 0) else [Nothing]

isDigit' :: String -> Bool
isDigit' [] = False
isDigit' [x] = isDigit x
isDigit' ('-' : xs) = isDigit' xs
isDigit' (x : xs) = isDigit x && isDigit' xs

isOpenCharList :: String -> Bool
isOpenCharList ('[' : _) = True
isOpenCharList _ = False

isOpenCharObject :: String -> Bool
isOpenCharObject ('{' : _) = True
isOpenCharObject _ = False

splitToTuples :: String -> [(String, Maybe JsonValue)]
splitToTuples s = map buildTuple (splitAcc ',' (Just s) [])

trim :: String -> String
trim = dropWhile isSpace

buildTuple :: String -> (String, Maybe JsonValue)
buildTuple [] = ("", Just (JString ""))
buildTuple s = let x : y : _ = splitAcc ':' (Just s) [] in (trim x, getJValue (trim y))

getJValue :: String -> Maybe JsonValue
getJValue xs
  | Data.Maybe.isJust (parseBool xs) = parseBool xs
  | Data.Maybe.isJust (parseNumber xs) = parseNumber xs
  | Data.Maybe.isJust (parseString xs) = parseString xs
  | Data.Maybe.isJust (parseList xs) = parseList xs
  | otherwise = Nothing

splitAcc :: Char -> Maybe String -> String -> [String]
splitAcc _ Nothing [] = []
splitAcc _ (Just []) [] = []
splitAcc _ (Just []) ys = [reverse ys]
splitAcc c (Just (x : xs)) ys
  | x == c = reverse ys : splitAcc c (Just xs) []
  | otherwise = splitAcc c (Just xs) (x : ys)

splitAcc' :: Char -> Maybe String -> String -> Int -> [String]
splitAcc' _ Nothing [] _ = []
splitAcc' _ (Just []) [] _ = []
splitAcc' _ (Just []) ys _ = [reverse ys]
splitAcc' c (Just (x : xs)) ys b
  | x == c && b == 0 = reverse ys : splitAcc' c (Just xs) [] b
  | x == '[' = splitAcc' c (Just xs) (x : ys) (b + 1)
  | x == ']' = splitAcc' c (Just xs) (x : ys) (b - 1)
  | otherwise = splitAcc' c (Just xs) (x : ys) b

example' :: String
example' = "{\"a\":10,\"isTrue\":true,\"name\":\"John\"}"

extractNodes :: String -> [(String, String)]
extractNodes xs = map parseKeyValuePairs (splitAcc ',' (Just xs) [])

parseKeyValuePairs :: String -> (String, String)
parseKeyValuePairs s = let x : y : _ = splitAcc ':' (Just s) [] in (x, y)

listString :: [String]
listString = ["\"favoriteColors\":[\"orange\"", "\"yellow\"", "\"red\"]"]
