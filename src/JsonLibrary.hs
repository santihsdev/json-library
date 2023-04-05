module JsonLibrary
  ( toJsonString,
  )
where

import JsonBuilder (parseJson, reverseExample, writeJson)
import JsonObject (JsonValue (JList, JObject, JString))

-- jsonValue = JObject [("name", JString "John"), ("lastName", JString "Doe")]

jsonValue' :: JsonValue
jsonValue' =
  JObject
    [ ("daysOfWeek", Just (JList [Just (JString "M"), Just (JString "T"), Just (JString "T")])),
      ("name", Just (JString "John")),
      ("lastName", Just (JString "Doe"))
    ]

stringValue :: String
stringValue = "{\"name\": \"test\", \"last\": \"test\"}"

-- jsonValue' = JObject [("daysOfWeek", Just (JList [Just (JString "M"), Just (JString "T"), Just (JString "T")])), ("name", Just (JString "John")), ("lastName", Just (JString "Doe"))]

toJsonString :: IO ()
-- toJsonString = putStrLn (writeJson (Just jsonValue'))
toJsonString = print (parseJson reverseExample)
