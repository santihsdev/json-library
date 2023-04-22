module JsonFuntor where

-- import Control.Applicative (Alternative (empty, (<|>)), Applicative (pure, (<*>)))
import Data.Char (isDigit, toUpper)
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

parseDouble :: Parser JsonValue
parseDouble = Parser divNumber

exampleBool :: Maybe (JsonValue, String)
exampleBool = parse parseBool "true hello"

exampleAppFunc =
  pure
    (\(JString xn, _) -> [toUpper x | x <- xn])
    <*> (parse parseString "\"hello \"world")

exampleAppFunc2 =
  pure (\(JBool a, _) (JBool b, _) (JBool c, _) -> (a && b || c, ""))
    <*> exampleBool
    <*> parse parseBool "false world"
    <*> parse parseBool "false hello"
