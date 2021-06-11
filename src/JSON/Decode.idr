module JSON.Decode

import Data.List
import Data.Vect
import Decidable.Equality
import Language.JSON.Data

%default total

public export
data Error 
  = Field String Error
  | Index Nat Error
  | OneOf (List Error)
  | Failure String JSON

expecting : String -> JSON -> Error

export
data Decoder a = MkDecoder (JSON -> Either Error a)

public export
Functor Decoder where
  map f (MkDecoder decode) = 
    MkDecoder (\jsonValue => map f (decode jsonValue))

public export
Applicative Decoder where
  pure value = MkDecoder (const (Right value))
  (<*>) (MkDecoder decodeFunc) (MkDecoder decodeArg) = 
    MkDecoder (\jsonValue => decodeFunc jsonValue <*> decodeArg jsonValue)

public export
Monad Decoder where
  join (MkDecoder decodeDecoder) = 
    MkDecoder (\jsonValue => 
      do (MkDecoder decode) <- (decodeDecoder jsonValue)
         decode jsonValue
    )

-- @TODO replace all occurences of 'andThen' with '>>='
-- @TODO make sure mapN is not referenced anymore
-- @TODO make sure Elm is not referenced anymore
-- @TODO separate out the primitives from the rest so that we cannot write decoders
--       in terms of MkDecoder anymore

-- Primitive Decoders

||| Ignore the JSON and make the decoder fail. This is handy when used with
||| `oneOf` or `>>=` where you want to give a custom error message in some
||| case.
public export
fail : String -> Decoder a
fail errorMsg = MkDecoder (\jsonValue => Left (Failure errorMsg jsonValue))

{-| Decode a JSON number into an Idris `Int`.
    decodeString int "true"              == Err ...
    decodeString int "42"                == Ok 42
    decodeString int "3.14"              == Err ...
    decodeString int "\"hello\""         == Err ...
    decodeString int "{ \"hello\": 42 }" == Err ...
-}
public export
int : Decoder Int
int = MkDecoder decodeInt
  where 
    decodeInt : JSON -> Either Error Int
    decodeInt jsonValue@(JNumber value) =
      -- If no information is lost during the round trip from double to int back to double,
      -- then value must have been an integer.
      if value == cast (the Int (cast value)) then 
        Right (cast value)
      else
        Left (expecting "an INT" jsonValue)
    decodeInt jsonValue = Left (expecting "an INT" jsonValue)

||| Decode a JSON boolean into an Idris `Bool`.
|||    decodeString bool "true"              == Ok True
|||    decodeString bool "42"                == Err ...
|||    decodeString bool "3.14"              == Err ...
|||    decodeString bool "\"hello\""         == Err ...
|||    decodeString bool "{ \"hello\": 42 }" == Err ...
public export
bool : Decoder Bool
bool = MkDecoder decodeBool
  where
    decodeBool : JSON -> Either Error Bool
    decodeBool (JBoolean value) = Right value
    decodeBool jsonValue        = Left (expecting "a BOOL" jsonValue)

||| Decode a JSON number into an Idris `Double`.
|||    decodeString double "true"              == Err ..
|||    decodeString double "42"                == Ok 42
|||    decodeString double "3.14"              == Ok 3.14
|||    decodeString double "\"hello\""         == Err ...
|||    decodeString double "{ \"hello\": 42 }" == Err ...
public export
double : Decoder Double
double = MkDecoder decodeDouble
  where
    decodeDouble : JSON -> Either Error Double
    decodeDouble (JNumber value) = Right value
    decodeDouble jsonValue       = Left (expecting "a DOUBLE" jsonValue)

||| Decode a JSON string into an Idris `String`.
|||    decodeString string "true"              == Err ...
|||    decodeString string "42"                == Err ...
|||    decodeString string "3.14"              == Err ...
|||    decodeString string "\"hello\""         == Ok "hello"
|||    decodeString string "{ \"hello\": 42 }" == Err ...
public export
string : Decoder String
string = MkDecoder decodeString
  where
    decodeString : JSON -> Either Error String
    decodeString (JString value) = Right value
    decodeString jsonValue       = Left (expecting "a STRING" jsonValue)

||| Decode a `null` value into some Idris value.
|||    decodeString (null False) "null" == Ok False
|||    decodeString (null 42) "null"    == Ok 42
|||    decodeString (null 42) "42"      == Err ..
|||    decodeString (null 42) "false"   == Err ..
||| So if you ever see a `null`, this will return whatever value you specified.
public export
null : a -> Decoder a
null value = MkDecoder decodeNull
  where
    decodeNull : JSON -> Either Error a
    decodeNull JNull     = Right value
    decodeNull jsonValue = Left (expecting "null" jsonValue)

-- DATA STRUCTURES

||| Try a bunch of different decoders. This can be useful if the JSON may come
||| in a couple different formats. For example, say you want to read an array of
||| numbers, but some of them are `null`.
|||    badInt : Decoder Int
|||    badInt =
|||      oneOf [ int, null 0 ]
|||    -- decodeString (list badInt) "[1,2,null,4]" == Ok [1,2,0,4]
||| Why would someone generate JSON like this? Questions like this are not good
||| for your health. The point is that you can use `oneOf` to handle situations
||| like this!
||| You could also use `oneOf` to help version your data. Try the latest format,
||| then a few older ones that you still support. You could use `>>=` to be
||| even more particular if you wanted.
public export
oneOf : List (Decoder a) -> Decoder a
oneOf decoders = MkDecoder (oneOfHelp decoders)
  where
    oneOfHelp : {default [] errors : List Error} -> List (Decoder a) -> JSON -> Either Error a
    oneOfHelp {errors} ((MkDecoder decode) :: decoders) jsonValue = 
      case decode jsonValue of
        Right value => Right value
        Left error  => oneOfHelp {errors=error::errors} decoders jsonValue
    oneOfHelp {errors} [] jsonValue = Left (OneOf (reverse errors))

||| Decode a nullable JSON value into an Idris value.
|||    decodeString (nullable int) "13"    == Ok (Just 13)
|||    decodeString (nullable int) "42"    == Ok (Just 42)
|||    decodeString (nullable int) "null"  == Ok Nothing
|||    decodeString (nullable int) "true"  == Err ..
public export
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  oneOf
    [ null Nothing
    , map Just decoder 
    ]

||| Decode a JSON array into an Idris `List`.
|||    decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
|||    decodeString (list bool) "[true,false]" == Ok [True,False]
public export
list : Decoder a -> Decoder (List a)
list (MkDecoder decodeElement) = MkDecoder decodeList
  where
    decodeListHelp : {default 0 idx: Nat} -> List JSON -> Either Error (List a)
    decodeListHelp [] = Right []
    decodeListHelp {idx} (x :: xs) = 
      case decodeElement x of
        Left err    => Left (Index idx err)
        Right value => map (value::) (decodeListHelp {idx = idx + 1} xs)

    decodeList : JSON -> Either Error (List a)
    decodeList (JArray xs) = decodeListHelp xs
    decodeList jsonValue = Left (expecting "a LIST" jsonValue)

||| Decode a JSON array into an Idris `List`.
|||    decodeString (list int) "[1,2,3]"       == Ok [1,2,3]
|||    decodeString (list bool) "[true,false]" == Ok [True,False]
public export
vect : Decoder a -> (len: Nat) -> Decoder (Vect len a)
vect (MkDecoder decodeElement) len = MkDecoder decodeVect
  where
    decodeVectHelp : {default 0 idx: Nat} -> List JSON -> Either Error (len' ** Vect len' a)
    decodeVectHelp [] = Right (0 ** [])
    decodeVectHelp {idx} (x :: xs) = 
      case decodeElement x of
        Left err    => Left (Index idx err)
        Right value => map (\(len' ** xs) => (S len' ** value::xs)) (decodeVectHelp {idx = idx + 1} xs)

    decodeVect : JSON -> Either Error (Vect len a)
    decodeVect jsonValue@(JArray xs) = 
      do (len' ** vect) <- decodeVectHelp xs
         case decEq len' len of
            (Yes prf)   => rewrite sym prf in Right vect
            (No contra) => Left (expecting ("a VECT with exactly" ++ show len ++ "elements") jsonValue)
    decodeVect jsonValue = Left (expecting "a VECT" jsonValue)

keyValuePairs : Decoder a -> Decoder (List (String, a))
keyValuePairs (MkDecoder decode) = MkDecoder decodeKeyValuePairs
  where
    decodeKeyValuePairsHelp : List (String, JSON) -> Either Error (List (String, a))
    decodeKeyValuePairsHelp [] = Right []
    decodeKeyValuePairsHelp ((field, jsonValue) :: entries) =
      case decode jsonValue of
        Left err    => Left (Field field err)
        Right value => map ((field, value)::) (decodeKeyValuePairsHelp entries)

    decodeKeyValuePairs : JSON -> Either Error (List (String, a))
    decodeKeyValuePairs (JObject entries) = decodeKeyValuePairsHelp entries
    decodeKeyValuePairs jsonValue         = Left (expecting "an OBJECT" jsonValue)




-- ||| Decode a JSON string into an Idris `String`.
-- |||    decodeString string "true"              == Err ...
-- |||    decodeString string "42"                == Err ...
-- |||    decodeString string "3.14"              == Err ...
-- |||    decodeString string "\"hello\""         == Ok "hello"
-- |||    decodeString string "{ \"hello\": 42 }" == Err ...
-- string : ()
-- string = ()