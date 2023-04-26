module JsonObject (JsonValue (..), NodeName) where

type NodeName = String

data JsonValue
  = JString String
  | JBool Bool
  | JObject [(String, Maybe JsonValue)]
  | JNumber Double
  | JList [JsonValue]
  | JNil
  deriving (Show)

comparator :: (Eq a) => [a] -> [a] -> [Bool]
comparator [] _ = []
comparator _ [] = []
comparator (x : xs) (y : ys)
  | x == y = True : comparator xs ys
  | otherwise = False : comparator xs ys

instance Eq JsonValue where
  (JString a) == (JString b) = a == b
  (JString _) == _ = False
  (JBool a) == (JBool b) = a == b
  (JBool _) == _ = False
  (JNumber a) == (JNumber b) = a == b
  (JNumber _) == _ = False
  (JList a) == (JList b) = and (comparator a b)
  (JList _) == _ = False
  JObject a == JObject b =
    and (comparator [x | (_, Just x) <- a] [y | (_, Just y) <- b])
      && and (comparator [s | (s, _) <- a] [s | (s, _) <- b])
  JObject _ == _ = False
  JNil == _ = False
