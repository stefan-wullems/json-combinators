module JSON.Decode

import Language.JSON

data Error = 
  Field String Error
  Index Nat Error
  OneOf (List Error)
  Failure String JSON

data Decoder a = MkDecoder (JSON -> Either Error a)
  
||| Decode a JSON string into an Idris `String`.
|||    decodeString string "true"              == Err ...
|||    decodeString string "42"                == Err ...
|||    decodeString string "3.14"              == Err ...
|||    decodeString string "\"hello\""         == Ok "hello"
|||    decodeString string "{ \"hello\": 42 }" == Err ...
string : ()
string = ()