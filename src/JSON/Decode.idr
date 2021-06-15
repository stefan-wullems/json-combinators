module JSON.Decode

import Data.List
import Data.Vect
import Data.Nat
import Decidable.Equality
import Language.JSON

%default total

public export
data Error 
  = Field String Error
  | Index Nat Error
  | OneOf (List Error)
  | Failure String JSON
  | InvalidJSON String

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
      do 
        (MkDecoder decode) <- (decodeDecoder jsonValue)
        decode jsonValue
    )

-- @TODO make sure each decoder enriches its errors with extra info if possible

--------------------------------------------------------------------------------
-- Running a decoder
--------------------------------------------------------------------------------

||| Parse the given string into a JSON value and then run the `Decoder` on it.
||| This will fail if the string is not well-formed JSON or if the `Decoder`
||| fails for some reason.
|||
||| ```idris example
||| > decodeString int "4"     
||| Right 4
||| ```
||| ```idris example
||| > decodeString int "1 + 2"
||| Left err
||| ```
decodeString : Decoder a -> String -> Either Error a
decodeString (MkDecoder decode) jsonString =
  case parse jsonString of
    Just jsonValue =>
      decode jsonValue

    Nothing =>
      Left (InvalidJSON jsonString)

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
||| ```idris example
||| > :let badInt : Decoder Int
||| > :let badInt = oneOf [ int, null 0 ]
|||
||| > decodeString (list badInt) "[1,2,null,4]"
||| Right [1,2,0,4]
||| ```
|||
||| Why would someone generate JSON like this? Questions like this are not good
||| for your health. The point is that you can use `oneOf` to handle situations
||| like this!
|||
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
||| ``` idris example
||| > decodeString int "42"                
||| Right 42
||| ```
||| ```idris example
||| > decodeString int "3.14"              
||| Left err
||| ```
||| ```idris example
||| > decodeString int "\"hello\""         
||| Left err
||| ```
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
||| ```idris example
||| > decodeString bool "true"              
||| Right True
||| ```
||| ```idris example
||| > decodeString bool "42"                 
||| Left err
||| ```
public export
bool : Decoder Bool
bool = MkDecoder decodeBool
  where
    decodeBool : JSON -> Either Error Bool
    decodeBool (JBoolean value) = Right value
    decodeBool jsonValue        = Left (expecting "a BOOL" jsonValue)

||| Decode a JSON number into an Idris `Double`.
|||
||| ```idris example
||| > decodeString double "true"              
||| Left err
||| ```
||| ```idris example
||| > decodeString double "42"
||| Right 42.0
||| ```
||| ```idris example
||| > decodeString double "3.14"
||| Right 3.14
||| ```
public export
double : Decoder Double
double = MkDecoder decodeDouble
  where
    decodeDouble : JSON -> Either Error Double
    decodeDouble (JNumber value) = Right value
    decodeDouble jsonValue       = Left (expecting "a DOUBLE" jsonValue)

||| Decode a JSON string into an Idris `String`.
|||
||| ```idris example
||| > decodeString string "true"
||| Left err
||| ```
||| ```idris example
||| > decodeString string "42"
||| Left err
||| ```
||| ```idris example
||| > decodeString string "\"hello\""
||| Right "hello"
||| ```
public export
string : Decoder String
string = MkDecoder decodeString
  where
    decodeString : JSON -> Either Error String
    decodeString (JString value) = Right value
    decodeString jsonValue       = Left (expecting "a STRING" jsonValue)

||| Decode a `null` value into some Idris value.
|||
||| ```idris example
||| > decodeString (null 42) "null"
||| Right 42
||| ```
||| ```idris example
||| > decodeString (null 42) "42"
||| Left err
||| ```
||| 
||| So if you ever see a `null`, this will return whatever value you specified.
public export
null : a -> Decoder a
null value = MkDecoder decodeNull
  where
    decodeNull : JSON -> Either Error a
    decodeNull JNull     = Right value
    decodeNull jsonValue = Left (expecting "null" jsonValue)

--------------------------------------------------------------------------------
-- Dealing with optionality
--------------------------------------------------------------------------------

||| Decode a nullable JSON value into an Idris value.
||| 
||| ```idris example
||| > decodeString (nullable int) "42"
||| Right (Just 42)
||| ```
||| ```idris example
||| > decodeString (nullable int) "null"
||| Right Nothing
||| ```
||| ```idris example
||| > decodeString (nullable int) "true" 
||| Left err
||| ```
public export
nullable : Decoder a -> Decoder (Maybe a)
nullable decoder =
  oneOf
    [ null Nothing
    , map Just decoder 
    ]

  

||| Helpful for dealing with optional fields. Here are a few slightly different
||| examples:
||| 
||| ```idris example
||| > decodeString (maybe (field "name" string)) "{ \"name\": \"tom\", \"age\": 42 }"
||| Right (Just "tom")
||| ```
||| ```idris example
||| > decodeString (maybe (field "age" string)) "{ \"name\": \"tom\", \"age\": 42 }" 
||| Right Nothing
||| ```
||| ```idris example
|||  > decodeString (maybe (field "height" double)) "{ \"name\": \"tom\", \"age\": 42 }"
|||  Right Nothing
||| ```
||| ```idris example
||| > decodeString (field "name" (maybe string)) "{ \"name\": \"tom\", \"age\": 42 }"
||| Right (Just "Tom")
||| ```
||| ```idris example
||| > decodeString (field "age" (maybe string)) "{ \"name\": \"tom\", \"age\": 42 }"
||| Right Nothing
||| ```idris example
||| > decodeString (field "height" (maybe double)) "{ \"name\": \"tom\", \"age\": 42 }"
||| Left err
||| ```
|||
||| Notice the last example! It is saying we *must* have a field named `height` and
||| the content *may* be a double. There is no `height` field, so the decoder fails.
|||
||| Point is, `maybe` will make exactly what it contains conditional. For optional
||| fields, this means you probably want it *outside* a use of `field` or `at`.
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
  oneOf
    [ map Just decoder
    , pure Nothing
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
      do 
        acc' <- func (x, idx) acc
        res <- fallibleIndexedFoldlHelp {idx=idx+1} xs acc'
        pure res

||| A variation of indexedMap that can fail and will short circuit to save resources if it does.
||| Failure is triggered by returning a (Left ..) from func.
fallibleIndexedMap : (func: (a, Nat) -> Either err b) -> (input: List a) -> Either err (List b)
fallibleIndexedMap func input = fallibleIndexedFoldl fallibleIndexedMapHelp [] input
  where
    fallibleIndexedMapHelp : (a, Nat) -> List b -> Either err (List b)
    fallibleIndexedMapHelp element xs = map (snoc xs) (func element)
         
||| Decode a JSON array into an Idris `List`.
||| 
||| ```idris example
||| > decodeString (list int) "[1,2,3]"
||| Right [1,2,3]
||| ```
||| ```idris example
||| > decodeString (list bool) "[true,false]"
||| Right [True,False]
||| ```
public export
list : Decoder a -> Decoder (List a)
list (MkDecoder decodeElement) = MkDecoder decodeList
  where
    decodeListHelp : (JSON, Nat) -> Either Error a
    decodeListHelp (jsonValue, idx) = 
      bimap
        {- Decoding failed    -} (\err => Index idx err)
        {- Decoding succeeded -} id 
        (decodeElement jsonValue)
    
    decodeList : JSON -> Either Error (List a)
    decodeList jsonValue = 
      expectArray jsonValue 
        >>= fallibleIndexedMap decodeListHelp

||| Decode a JSON array, requiring a particular index.
|||
||| ```idris example
||| > decodeString (index 0 string) "[ \"alice\", \"bob\", \"chuck\" ]"
||| Right "alice"
||| ```
||| ```idris example
||| > decodeString (index 1 string) "[ \"alice\", \"bob\", \"chuck\" ]"
||| Right "bob"
||| ```
||| ```idris example
||| > decodeString (index 2 string) "[ \"alice\", \"bob\", \"chuck\" ]"
||| Right "chuck"
||| ```
||| ```idris example
||| > decodeString (index 3 string) "[ \"alice\", \"bob\", \"chuck\" ]"
||| Left err
||| ```
public export
index : (idx: Nat) -> Decoder a -> Decoder a
index idx (MkDecoder decode) = MkDecoder decodeIndex
  where
    decodeIndex : JSON -> Either Error a
    decodeIndex jsonValue =
      do xs <- expectArray jsonValue
         case inBounds idx xs of
            Yes prf => 
              bimap
               {- Decoding failed    -} (\err => Index idx err)
               {- Decoding succeeded -} id
               (decode (index idx xs))

            No contra => 
              Left (
                expecting 
                  ("a LONGER array. Need index " ++ show idx ++ " but only see " ++ show (length xs) ++ " entries") 
                  jsonValue
              )

||| Decode a JSON array into a dependent pair of an idris "Vect" dependent on its length.
|||
||| ```idris example 
||| > decodeString (vect int) "[1,2,3]"
||| Right (3 ** [1, 2, 3])
||| ```
||| ```idris example
||| > decodeString (vect bool) "[true,false]"
||| Right (2 ** [True,False])
||| ```
public export
vect : Decoder a -> Decoder (len ** Vect len a)
vect (MkDecoder decodeElement) = MkDecoder decodeVect
  where
    decodeVectHelp : (JSON, Nat) -> (len ** Vect len a) -> Either Error (len' ** Vect len' a)
    decodeVectHelp (jsonValue, idx) (len ** xs) =
      bimap
        {- Decoding failed    -} (\err => Index idx err)
        {- Decoding succeeded -} (\x   => (S len ** x :: xs))
        (decodeElement jsonValue)

    decodeVect : JSON -> Either Error (len ** Vect len a)
    decodeVect jsonValue = 
      expectArray jsonValue
        >>= fallibleIndexedFoldl decodeVectHelp (0 ** [])

||| Decode a JSON array into an Idris `Vect` with an exact length.
||| 
||| ```idris example
||| > decodeString (vectExact 3 int)  "[1,2,3]"
||| Right [1,2,3]
||| ```
||| ```idris example
||| > decodeString (vectExact 2 bool) "[true,false]"
||| Right [True,False]
||| ```
||| ```idris example
||| > decodeString (vectExact 5 bool) "[true,false]"
||| Left err
||| ```
public export
vectExact : (len: Nat) -> Decoder a -> Decoder (Vect len a)
vectExact len decoder = 
  do 
    (len' ** xs) <- vect decoder
    case decEq len' len of
      Yes Refl => 
        pure xs

      No contra => 
        fail ("Expecting an ARRAY with exactly" ++ show len ++ "elements") 

||| Decode a JSON array into a dependent pair of an Idris `Vect` dependent on
||| the amount of elements it has additionally to the minimum amount.
||| 
||| ```idris example
||| > decodeString (vectAtLeast 3 int) "[1,2,3]" 
||| Right (0 ** [1,2,3])
||| ```
||| ```idris example
||| > decodeString (vectAtLeast 1 int) "[1,2,3]"
||| Right (2 ** [1,2,3])
||| ```
||| ```idris example
||| > decodeString (vectAtLeast 1 bool) "[true,false]"
||| Right (1 ** [True,False])
||| ```
||| ```idris example
||| > decodeString (vectAtLeast 3 bool) "[true,false]"
||| Left err
||| ```
public export
vectAtLeast : (len: Nat) -> Decoder a -> Decoder (rest ** Vect (len + rest) a)
vectAtLeast len decoder = 
  do 
    (len' ** xs) <- vect decoder
    case isLTE len len' of
      Yes prfLte => 
        let (rest ** Refl) = diff len len' prfLte
        in pure (rest ** xs)

      No contra => 
        fail ("Expecting an ARRAY with at least" ++ show len ++ "elements")
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
||| ```idris example
||| > decodeString (keyValuePairs int) "{ \"alice\": 42, \"bob\": 99 }"
||| Right [("alice", 42), ("bob", 99)]
||| ```
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
||| ```idris example
||| > decodeString (field "x" int) "{ \"x\": 3, \"y\": 4 }"
||| Right 3
||| ```
||| ```idris example
||| > decodeString (field "x" int) "{ \"x\": true }" 
||| Left err
||| ```
||| ```idris example
||| decodeString (field "x" int) "{ \"y\": 4 }" 
||| Left err
||| ```
|||
||| The object *can* have other fields. Lots of them! The only thing this decoder
||| cares about is if `x` is present and that the value there is an `Int`.
public export
field : String -> Decoder a -> Decoder a
field fieldName (MkDecoder decode) = MkDecoder decodeField
  where
    decodeFieldHelp : (String, JSON) -> Either Error a
    decodeFieldHelp (fieldName, jsonValue) =
       bimap
        {- Decoding failed    -} (\err => Field fieldName err)
        {- Decoding succeeded -} id
        (decode jsonValue)

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
||| ```idris example
||| > decodeString (at ["person", "name"] string) "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
||| Right "tom"
||| ```
||| ```idris example
||| > decodeString (at ["person", "age" ] int) "{ \"person\": { \"name\": \"tom\", \"age\": 42 } }"
||| Right 42
||| ```
public export
at : (path: List String) -> Decoder a -> Decoder a
at path decoder =
  foldr field decoder path

||| If no path is given the decoder is applied on the spot.
public export
atEmptyListSimplyReturnsDecoder : (decoder: Decoder a) -> at [] decoder = decoder
atEmptyListSimplyReturnsDecoder decoder = Refl

||| `at` can be seen as a bunch of nested calls to `field`.
public export
atShorthandForNestedField : {decoder: Decoder a} -> 
                            {fieldName: String} ->
                            {fieldNames: List String} ->
                            at (fieldName::fieldNames) decoder = field fieldName (at fieldNames decoder)
atShorthandForNestedField = Refl
