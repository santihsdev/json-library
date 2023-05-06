import JsonFunctor (getJLine)
import JsonObject (JsonValue (JBool, JList, JNumber, JObject, JString))
import Test.HUnit
  ( Test (TestCase),
    Testable (test),
    assertBool,
    assertEqual,
    runTestTT,
  )

main :: IO ()
main = do
  counts <- runTestTT testAddition
  print counts

testAddition :: Test
testAddition =
  test
    [ TestCase (assertEqual "true will be JBool True" (getJLine "true") (JBool True)),
      TestCase
        (assertBool "true will be JBool False is not the same" (getJLine "true" /= JBool False)),
      TestCase
        (assertEqual "The name will be positive" (getJLine "123") (JNumber 123)),
      TestCase (assertBool "The name will be the same as a JsonValue" (getJLine "123" == JNumber 123)),
      TestCase (assertBool "The name will not be the same as a JsonValue" (getJLine "123" /= JNumber 12)),
      TestCase (assertBool "The string must be the same as the JsonValue" (getJLine "\"hello\"" == JString "hello")),
      TestCase (assertBool "The string must not be the same as the JNumber" (getJLine "\"hello\"" /= JNumber 12)),
      TestCase (assertBool "The List is the as the JList" (getJLine "[1,2,3]" == JList [JNumber 1, JNumber 2, JNumber 3])),
      TestCase (assertBool "The List is the as the JList with a JString" (getJLine "[1,2,3]" /= JList [JNumber 1, JNumber 2, JString "Hello world"])),
      TestCase (assertBool "Creating a JObject" (getJLine "{hello:124}" == JObject [("hello", Just (JNumber 124))])),
      TestCase (assertBool "Creating a JObject and comparing with a same JObject" (getJLine "{hello:\"world\"}" /= JObject [("hello", Just (JNumber 124))])),
      TestCase (assertBool "Creating a JObject and they are not equals due to the key" (getJLine "{hello:\"world\"}" /= JObject [("hell", Just (JNumber 124))]))
    ]
