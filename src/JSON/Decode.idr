module JSON.Decode

import Data.List
import Data.Vect
import Data.Nat
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
-- @TODO make sure each decoder enriches its errors with extra info if possible

||| Ignore the JSON and make the decoder fail. This is handy when used with
||| `oneOf` or `>>=` where you want to give a custom error message in some
||| case.
public export
fail : String -> Decoder a
fail errorMsg = MkDecoder (\jsonValue => Left (Failure errorMsg jsonValue))

||| Try a bunch of different decoders. This can be useful if the JSON may come
||| in a couple different formats. For example, say you want to read an array of
||| numbers, but some of them are `null`.
|||
|||    badInt : Decoder Int
|||    badInt =
|||      oneOf [ int, null 0 ]
|||
|||    decodeString (list badInt) "[1,2,null,4]" == Right [1,2,0,4]
|||
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

--------------------------------------------------------------------------------
-- JSON primitives
--------------------------------------------------------------------------------

||| Decode a JSON number into an Idris `Int`.
|||
|||    decodeString int "true"              == Left err
|||    decodeString int "42"                == Right 42
|||    decodeString int "3.14"              == Left err
|||    decodeString int "\"hello\""         == Left err
|||    decodeString int "{ \"hello\": 42 }" == Left err
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
|||
|||    decodeString bool "true"              == Right True
|||    decodeString bool "42"                == Left err
|||    decodeString bool "3.14"              == Left err
|||    decodeString bool "\"hello\""         == Left err
|||    decodeString bool "{ \"hello\": 42 }" == Left err
public export
bool : Decoder Bool
bool = MkDecoder decodeBool
  where
    decodeBool : JSON -> Either Error Bool
    decodeBool (JBoolean value) = Right value
    decodeBool jsonValue        = Left (expecting "a BOOL" jsonValue)

||| Decode a JSON number into an Idris `Double`.
|||
|||    decodeString double "true"              == Left err
|||    decodeString double "42"                == Right 42
|||    decodeString double "3.14"              == Right 3.14
|||    decodeString double "\"hello\""         == Left err
|||    decodeString double "{ \"hello\": 42 }" == Left err
public export
double : Decoder Double
double = MkDecoder decodeDouble
  where
    decodeDouble : JSON -> Either Error Double
    decodeDouble (JNumber value) = Right value
    decodeDouble jsonValue       = Left (expecting "a DOUBLE" jsonValue)

||| Decode a JSON string into an Idris `String`.
|||
|||    decodeString string "true"              == Left err
|||    decodeString string "42"                == Left err
|||    decodeString string "3.14"              == Left err
|||    decodeString string "\"hello\""         == Right "hello"
|||    decodeString string "{ \"hello\": 42 }" == Left err
public export
string : Decoder String
string = MkDecoder decodeString
  where
    decodeString : JSON -> Either Error String
    decodeString (JString value) = Right value
    decodeString jsonValue       = Left (expecting "a STRING" jsonValue)

||| Decode a `null` value into some Idris value.
|||
|||    decodeString (null False) "null" == Right False
|||    decodeString (null 42) "null"    == Right 42
|||    decodeString (null 42) "42"      == Left err
|||    decodeString (null 42) "false"   == Left err
||| 
||| So if you ever see a `null`, this will return whatever value you specified.
public export
null : a -> Decoder a
null value = MkDecoder decodeNull
  where
    decodeNull : JSON -> Either Error a
    decodeNull JNull     = Right value
    decodeNull jsonValue = Left (expecting "null" jsonValue)

||| Decode a nullable JSON value into an Idris value.
||| 
|||    decodeString (nullable int) "13"    == Right (Just 13)
|||    decodeString (nullable int) "42"    == Right (Just 42)
|||    decodeString (nullable int) "null"  == Right Nothing
|||    decodeString (nullable int) "true"  == Left err
public export
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  oneOf
    [ null Nothing
    , map Just decoder 
    ]

--------------------------------------------------------------------------------
-- JSON arrays
--------------------------------------------------------------------------------

expectArray : JSON -> Either Error (List JSON)
expectArray (JArray xs) = Right xs
expectArray jsonValue   = Left (expecting "an ARRAY" jsonValue)

||| A variation of indexedFoldl that can fail and will short circuit to save resources if it does.
||| Failure is triggered by returning a (Left ..) from func.
fallibleIndexedFoldl : (func: (a, Nat) -> b -> Either err b) -> 
                     (init: b) ->
                     (input : List a) ->
                     Either err b
fallibleIndexedFoldl func init input = fallibleIndexedFoldlHelp input init
  where
    fallibleIndexedFoldlHelp : {default 0 idx: Nat} -> (input: List a) -> (acc: b) -> Either err b
    fallibleIndexedFoldlHelp [] acc = Right acc
    fallibleIndexedFoldlHelp {idx = idx} (x :: xs) acc = 
      do acc' <- func (x, idx) acc
         res <- fallibleIndexedFoldlHelp {idx=idx+1} xs acc'
         pure res

||| A variation of indexedMap that can fail and will short circuit to save resources if it does.
||| Failure is triggered by returning a (Left ..) from func.
fallibleIndexedMap : (func: (a, Nat) -> Either err b) -> (input: List a) -> Either err (List b)
fallibleIndexedMap func input = fallibleIndexedFoldl fallibleIndexedMapHelp [] input
  where
    fallibleIndexedMapHelp : (a, Nat) -> List b -> Either err (List b)
    fallibleIndexedMapHelp element xs = 
      do res <- func element
         pure (xs ++ [res])

||| Decode a JSON array into an Idris `List`.
||| 
|||    decodeString (list int) "[1,2,3]"       == Right [1,2,3]
|||    decodeString (list bool) "[true,false]" == Right [True,False]
public export
list : Decoder a -> Decoder (List a)
list (MkDecoder decodeElement) = MkDecoder decodeList
  where
    decodeListHelp : (JSON, Nat) -> Either Error a
    decodeListHelp (jsonValue, idx) = 
      case decodeElement jsonValue of
        Left err => 
          Left (Index idx err)

        Right value => 
          Right value

    decodeList : JSON -> Either Error (List a)
    decodeList jsonValue = 
      expectArray jsonValue 
        >>= fallibleIndexedMap decodeListHelp

||| Decode a JSON array into a dependent pair of an idris "VECT" dependent on its length.
||| 
|||    decodeString (vect int) "[1,2,3]"       == Right (3 ** [1, 2, 3])
|||    decodeString (vect bool) "[true,false]" == Right (2 ** [True,False])
public export
vect : Decoder a -> Decoder (len ** Vect len a)
vect (MkDecoder decodeElement) = MkDecoder decodeVect
  where
    decodeVectHelp : (JSON, Nat) -> (len ** Vect len a) -> Either Error (len' ** Vect len' a)
    decodeVectHelp (jsonValue, idx) (len ** xs) = 
      case decodeElement jsonValue of
        Left err => 
          Left (Index idx err)

        Right x => 
          Right (S len ** x :: xs)

    decodeVect : JSON -> Either Error (len ** Vect len a)
    decodeVect jsonValue = 
      expectArray jsonValue
        >>= fallibleIndexedFoldl decodeVectHelp (0 ** [])

||| Decode a JSON array into an Idris `Vect` with an exact length.
||| 
|||    decodeString (vectExact 3 int)  "[1,2,3]"       == Right [1,2,3]
|||    decodeString (vectExact 2 bool) "[true,false]"  == Right [True,False]
|||    decodeString (vectExact 5 bool) "[true,false]"  == Left err
public export
vectExact : (len: Nat) -> Decoder a -> Decoder (Vect len a)
vectExact len decoder = 
  do (len' ** xs) <- vect decoder
     case decEq len' len of
        (Yes Refl)  => pure xs
        (No contra) => fail ("Expecting an ARRAY with exactly" ++ show len ++ "elements") 

||| Decode a JSON array into a dependent pair of an Idris `Vect` dependent on
||| the amount of elements it has additionally to the minimum amount.
||| 
|||    decodeString (vectAtLeast 3 int)  "[1,2,3]"       == Right (0 ** [1,2,3])
|||    decodeString (vectAtLeast 1 int)  "[1,2,3]"       == Right (2 ** [1,2,3])
|||    decodeString (vectAtLeast 1 bool) "[true,false]"  == Right (1 ** [True,False])
|||    decodeString (vectAtLeast 3 bool) "[true,false]"  == Left err
public export
vectAtLeast : (len: Nat) -> Decoder a -> Decoder (rest ** Vect (len + rest) a)
vectAtLeast len decoder = 
  do (len' ** xs) <- vect decoder
     case isLTE len len' of
       (Yes prfLte) => 
          let (rest ** Refl) = diff len len' prfLte
          in pure (rest ** xs)
       (No contra) => fail ("Expecting an ARRAY with at least" ++ show len ++ "elements")
  where
    diff : (x, y: Nat) -> (prf: LTE x y) -> (z ** x + z = y)
    diff 0 0 prf = (0 ** Refl)
    diff 0 y prf = (y ** Refl)
    diff (S k) 0 prf impossible
    diff (S k) (S j) prf = 
      let (rest ** prf) = diff k j (fromLteSucc prf)
      in (rest ** cong S prf)

--------------------------------------------------------------------------------
-- JSON objects
--------------------------------------------------------------------------------

expectObject : JSON -> Either Error (List (String, JSON))
expectObject (JObject entries) = Right entries
expectObject jsonValue         = Left (expecting "an OBJECT" jsonValue)

||| Decode a JSON object into an Idris `List` of pairs.
||| 
|||    decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
|||      == Right [("alice", 42), ("bob", 99)]
public export
keyValuePairs : Decoder a -> Decoder (List (String, a))
keyValuePairs (MkDecoder decode) = MkDecoder decodeKeyValuePairs
  where
    decodeKeyValuePairsHelp : List (String, JSON) -> Either Error (List (String, a))
    decodeKeyValuePairsHelp [] = Right []
    decodeKeyValuePairsHelp ((fieldName, jsonValue) :: entries) =
      case decode jsonValue of
        -- Enrich error with information on which field we attempted to decode
        Left err => 
          Left (Field fieldName err)

        Right value => 
          map ((fieldName, value)::) (decodeKeyValuePairsHelp entries)

    decodeKeyValuePairs : JSON -> Either Error (List (String, a))
    decodeKeyValuePairs jsonValue = 
      expectObject jsonValue 
        >>= decodeKeyValuePairsHelp

||| Decode a JSON object, requiring a particular field.
||| 
|||    decodeString (field "x" int) "{ \"x\": 3 }"            == Right 3
|||    decodeString (field "x" int) "{ \"x\": 3, \"y\": 4 }"  == Right 3
|||    decodeString (field "x" int) "{ \"x\": true }"         == Left err
|||    decodeString (field "x" int) "{ \"y\": 4 }"            == Left err
|||    decodeString (field "name" string) "{ \"name\": \"tom\" }" == Right "tom"
|||
||| The object *can* have other fields. Lots of them! The only thing this decoder
||| cares about is if `x` is present and that the value there is an `Int`.
public export
field : String -> Decoder a -> Decoder a
field fieldName (MkDecoder decode) = MkDecoder decodeField
  where
    decodeFieldHelp : (String, JSON) -> Either Error a
    decodeFieldHelp (fieldName, jsonValue) =
      case decode jsonValue of
        -- Enrich error with information on which field we attempted to decode
        Left err => 
          Left (Field fieldName err)

        Right value => 
          Right value

    decodeField : JSON -> Either Error a
    decodeField jsonValue =
      do entries <- expectObject jsonValue
         case find ((fieldName ==) . fst) entries of
            Just entry => 
              decodeFieldHelp entry

            Nothing => 
              Left (expecting ("an OBJECT with a field named `" ++ fieldName ++ "`") jsonValue)
           
||| Decode a nested JSON object, requiring certain fields.
|||
|||    json = """{ "person": { "name": "tom", "age": 42 } }"""
|||
|||    decodeString (at ["person", "name"] string) json  == Ok "tom"
|||    decodeString (at ["person", "age" ] int   ) json  == Ok 42
at : (path: List String) -> Decoder a -> Decoder a
at path decoder =
  foldr field decoder path

||| If no path is given the decoder is applied on the spot.
atEmptyListSimplyReturnsDecoder : (decoder: Decoder a) -> at [] decoder = decoder
atEmptyListSimplyReturnsDecoder decoder = Refl

||| At can be seen as a bunch of nested calls to `field`.
atShorthandForNestedField : {decoder: Decoder a} -> 
                            {fieldName: String} ->
                            {fieldNames: List String} ->
                            at (fieldName::fieldNames) decoder = field fieldName (at fieldNames decoder)
atShorthandForNestedField = Refl

