module Decode

import JSON.Decode
import Data.Vect

main : IO ()
main = 
  do
    -- JSON.Decode.decodeString
    printLn $ decodeString int "4"
    printLn $ decodeString int "1 + 2"

    -- JSON.Decode.oneOf
    let badInt = oneOf [ int, null 0 ]
    printLn $ decodeString (list badInt) "[1,2,null,4]"

    -- JSON.Decode.bool
    printLn $ decodeString bool "true"
    printLn $ decodeString bool "42"

    -- JSON.Decode.double
    printLn $ decodeString double "true"
    printLn $ decodeString double "42"
    printLn $ decodeString double "3.14"
    
    -- JSON.Decode.int
    printLn $ decodeString int "42"
    printLn $ decodeString int "3.14"
    printLn $ decodeString int "\"hello\""

    -- JSON.Decode.string
    printLn $ decodeString string "true"
    printLn $ decodeString string "42"
    printLn $ decodeString string "\"hello\""

    -- JSON.Decode.null
    printLn $ decodeString (null 42) "null"
    printLn $ decodeString (null 42) "42"

    -- JSON.Decode.nullable
    printLn $ decodeString (nullable int) "42"
    printLn $ decodeString (nullable int) "null"
    printLn $ decodeString (nullable int) "true" 

    -- JSON.Decode.maybe
    printLn $ decodeString (maybe (field "name" string)) "{ \"name\": \"tom\", \"age\": 42 }"
    printLn $ decodeString (maybe (field "age" string)) "{ \"name\": \"tom\", \"age\": 42 }"
    printLn $ decodeString (maybe (field "height" double)) "{ \"name\": \"tom\", \"age\": 42 }"
    printLn $ decodeString (field "name" (maybe string)) "{ \"name\": \"tom\", \"age\": 42 }"
    printLn $ decodeString (field "age" (maybe string)) "{ \"name\": \"tom\", \"age\": 42 }"
    printLn $ decodeString (field "height" (maybe double)) "{ \"name\": \"tom\", \"age\": 42 }"

    -- JSON.Decode.list
    printLn $ decodeString (list int) "[1,2,3]"
    printLn $ decodeString (list bool) "[true,false]"

    -- JSON.Decode.index
    printLn $ decodeString (index 0 string) "[ \"alice\", \"bob\", \"chuck\" ]"
    printLn $ decodeString (index 1 string) "[ \"alice\", \"bob\", \"chuck\" ]"
    printLn $ decodeString (index 2 string) "[ \"alice\", \"bob\", \"chuck\" ]"
    printLn $ decodeString (index 3 string) "[ \"alice\", \"bob\", \"chuck\" ]"

    -- JSON.Decode.vect
    printLn $ decodeString (vect int) "[1,2,3]"
    printLn $ decodeString (vect bool) "[true,false]"

    -- JSON.Decode.vectExact
    printLn $ decodeString (vectExact 3 int) "[1,2,3]"
    printLn $ decodeString (vectExact 2 bool) "[true,false]"
    printLn $ decodeString (vectExact 5 bool) "[true,false]"

    -- JSON.Decode.vectAtLeast
    printLn $ decodeString (vectAtLeast 3 int) "[1,2,3]"
    printLn $ decodeString (vectAtLeast 1 int) "[1,2,3]"
    printLn $ decodeString (vectAtLeast 1 bool) "[true,false]"
    printLn $ decodeString (vectAtLeast 3 bool) "[true,false]"

    -- JSON.Decode.keyValuePairs
    printLn $ decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"

    -- JSON.Decode.field
    printLn $ decodeString (field "x" int) "{ \"x\": 3, \"y\": 4 }"
    printLn $ decodeString (field "x" int) "{ \"x\": true }"
    printLn $ decodeString (field "x" int) "{ \"y\": 4 }"

    -- JSON.Decode.at
    printLn $ decodeString (at ["person", "name"] string) "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
    printLn $ decodeString (at ["person", "age" ] int) "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
