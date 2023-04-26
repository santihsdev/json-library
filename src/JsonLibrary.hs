module JsonLibrary
  ( toJsonString,
  )
where

import JsonFunctor (exampleJson, exampleJson2, getJLine)

toJsonString :: IO ()
toJsonString = do
  print (getJLine exampleJson)
  print (getJLine exampleJson2)
