module JsonLibrary
  ( toJsonString,
  )
where

import JsonBuilder (parseJson, reverseExample, exampleObject, writeJson, example)
import JsonObject (JsonValue (JList, JObject, JString))

jsonValue' :: JsonValue
jsonValue' =
  JObject
    [ ("daysOfWeek", Just (JList [Just (JString "M"), Just (JString "T"), Just (JString "T")])),
      ("name", Just (JString "John")),
      ("lastName", Just (JString "Doe"))
    ]

toJsonString :: IO ()
toJsonString = do
  print (parseJson reverseExample)
  print (parseJson exampleObject)
  putStrLn (writeJson (Just example))
