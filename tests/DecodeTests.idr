module Tests.DecodeTests

tests : List (String, Bool)
tests = 
  [ ("decodeString 1", decodeString int "4" == Right 4)
  , ("decodeString 2", isLeft (decodeString int "1 + 2") )
  , ("oneOf", 
    let badInt = oneOf [int, null 0]
    in decodeString (list badInt) "[1,2,null,4]") == Right [1,2,0,4]
  ]

data Status = Success | Failure

Show Status where
  show Success = "Success"
  show Failure = "Failure"

test : IO ()
test = foldr testHelp (pure Succeeded) tests
  where
    checkTest : (String, Bool) -> IO Status
    checkTest (description, result) =
      do
        let status = if result then Success else Failure
        putStrLn (description ++ ": " ++ (show status))
        pure status

    testHelp : List (String, Bool) -> Status -> IO ()
    testHelp [] Success = putStrLn "All tests have passed!"
    testHelp [] Failure = putStrLn "Some tests failed."
    testHelp (test :: tests) currentStatus = 
      do
        testStatus <- checkTest test

        let nextStatus = 
          case currentStatus of
            Success => testStatus
            Failure => Failure
        
        testHelp tests nextStatus
        
        