module Encode

import Data.Vect

import JSON.Encode

main : IO ()
main = 
  do
    -- JSON.Encode.encode
    let tom = object [ ( "name", string "Tom" ) , ( "age", int 42 ) ]
    printLn $ encode tom
    printLn $ encode {indentation=4} tom

    -- JSON.Encode.string
    printLn $ encode (string "")
    printLn $ encode (string "abc")

    -- JSON.Encode.int
    printLn $ encode (int 42)
    printLn $ encode (int (-7))

    -- JSON.Encode.double
    printLn $ encode (double 3.14)
    printLn $ encode (double (-42))

    -- JSON.Encode.bool
    printLn $ encode (bool True)
    printLn $ encode (bool False)

    -- JSON.Encode.null
    printLn $ encode null

    -- JSON.Encode.list
    printLn $ encode (list int [1,3,4])
    printLn $ encode (list bool [True,False])

    -- JSON.Encode.vect
    printLn $ encode (vect int [1,3,4])
    printLn $ encode (vect bool [True,False])

    -- JSON.Encode.object
    printLn $ encode (object [ ( "name", Encode.string "Tom" ) , ( "age", Encode.int 42 ) ])

