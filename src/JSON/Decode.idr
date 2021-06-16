module JSON.Decode

import Data.List
import Data.Vect
import Data.Nat
import Decidable.Equality
import Language.JSON

import JSON.Decode.Error

%default total

--------------------------------------------------------------------------------
-- JSON validation
--------------------------------------------------------------------------------

expectJSONNumber : JSON -> Either Error Double
expectJSONNumber (JNumber value) = Right value
expectJSONNumber jsonValue       = Left (Failure "Expecting a NUMBER" jsonValue)

expectJSONBool : JSON -> Either Error Bool
expectJSONBool (JBoolean value) = Right value
expectJSONBool jsonValue        = Left (Failure "Expecting a BOOL" jsonValue)

expectJSONString : JSON -> Either Error String
expectJSONString (JString value) = Right value
expectJSONString jsonValue       = Left (Failure "Expecting a STRING" jsonValue)

expectJSONNull : JSON -> Either Error ()
expectJSONNull JNull     = Right ()
expectJSONNull jsonValue = Left (Failure "Expecting the literal `null`" jsonValue)

expectJSONArray : JSON -> Either Error (List JSON)
expectJSONArray (JArray xs) = Right xs
expectJSONArray jsonValue   = Left (Failure "Expecting an ARRAY" jsonValue)

expectJSONObject : JSON -> Either Error (List (String, JSON))
expectJSONObject (JObject entries) = Right entries
expectJSONObject jsonValue         = Left (Failure "Expecting an OBJECT" jsonValue)

--------------------------------------------------------------------------------
-- Decoder & interface implementations
--------------------------------------------------------------------------------

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
public export
decodeString : Decoder a -> String -> Either Error a
decodeString (MkDecoder decode) jsonString =
  case parse jsonString of
    Just jsonValue =>
      decode jsonValue

    Nothing =>
      Left (InvalidJSONString jsonString)

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

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

oneOfSingleDecoder : (decoder: Decoder a) -> oneOf [decoder] = decoder
oneOfSingleDecoder decoder = ?bla

--------------------------------------------------------------------------------
-- JSON primitives
--------------------------------------------------------------------------------

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
bool = MkDecoder expectJSONBool

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
double = MkDecoder expectJSONNumber

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
    isInteger : Double -> Bool
    isInteger number =
      -- If no information is lost during the round trip from double to int back to double,
      -- then value must have been an integer.
      number == cast (the Int (cast number))

    decodeInt : JSON -> Either Error Int
    decodeInt jsonValue =
      do
        number <- expectJSONNumber jsonValue
        
        case isInteger number of
          True => 
            Right (cast number)

          False => 
            Left (Failure "Expecting the number to be an INTEGER" jsonValue)
          

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
string = MkDecoder expectJSONString

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
null value = map (const value) (MkDecoder expectJSONNull)

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
public export
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
  oneOf
    [ map Just decoder
    , pure Nothing
    ]

--------------------------------------------------------------------------------
-- JSON arrays
--------------------------------------------------------------------------------

||| A generalization of indexedFoldl that can fail and will short circuit to save resources if it does.
||| Failure is triggered by returning a (Left ..) from func.
fallibleIndexedFoldr : (func: b -> (a, Nat) -> Either err b) -> 
                       (init: b) ->
                       (input : List a) ->
                       Either err b
fallibleIndexedFoldr func init input = fallibleIndexedFoldrHelp init input
  where
    fallibleIndexedFoldrHelp : {default 0 idx: Nat} -> (init: b) -> (input: List a) -> Either err b
    fallibleIndexedFoldrHelp {idx} init [] = Right init
    fallibleIndexedFoldrHelp {idx} init (x :: xs) = 
      do 
        acc <- fallibleIndexedFoldrHelp {idx=idx+1} init xs
        func acc (x, idx) 

||| A generalization of indexedMap that can fail and will short circuit to save resources if it does.
||| Failure is triggered by returning a (Left ..) from func.
fallibleIndexedMap : (func: (a, Nat) -> Either err b) -> (input: List a) -> Either err (List b)
fallibleIndexedMap func input = 
  fallibleIndexedFoldr (\xs, element => map (::xs) (func element)) [] input

         
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
list (MkDecoder decode) = MkDecoder decodeList
  where
    decodeListHelp : (JSON, Nat) -> Either Error a
    decodeListHelp (jsonValue, idx) = 
      mapFst (\err => Index idx err) (decode jsonValue)
    
    decodeList : JSON -> Either Error (List a)
    decodeList jsonValue = 
      expectJSONArray jsonValue 
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
      do 
        xs <- expectJSONArray jsonValue
        case inBounds idx xs of
          Yes prf => 
            mapFst (\err => Index idx err) (decode (index idx xs))

          No contra => 
            Left (
              Failure 
                ("Expecting a LONGER array. Need index " ++ show idx ++ " but only see " ++ show (length xs) ++ " entries") 
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
vect (MkDecoder decode) = MkDecoder decodeVect
  where
    decodeVectHelp :  (len ** Vect len a) -> (JSON, Nat) -> Either Error (len' ** Vect len' a)
    decodeVectHelp (len ** xs) (jsonValue, idx) =
      bimap
        (\err => Index idx err)
        (\result => (S len ** result :: xs))
        (decode jsonValue)

    decodeVect : JSON -> Either Error (len ** Vect len a)
    decodeVect jsonValue = 
      expectJSONArray jsonValue
        >>= fallibleIndexedFoldr decodeVectHelp (0 ** [])

||| Decode a JSON array into an Idris `Vect` with an exact length.
||| 
||| ```idris example
||| > decodeString (vectExact 3 int) "[1,2,3]"
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
        fail ("Expecting an ARRAY with exactly " ++ show len ++ " elements") 

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
        fail ("Expecting an ARRAY with at least " ++ show len ++ " elements")
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
        Left err => 
          Left (Field fieldName err)

        Right value => 
          map ((fieldName, value)::) (decodeKeyValuePairsHelp entries)

    decodeKeyValuePairs : JSON -> Either Error (List (String, a))
    decodeKeyValuePairs jsonValue = 
      expectJSONObject jsonValue 
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
      mapFst (\err => Field fieldName err) (decode jsonValue)

    decodeField : JSON -> Either Error a
    decodeField jsonValue =
      do entries <- expectJSONObject jsonValue
         case find ((fieldName ==) . fst) entries of
            Just entry => 
              decodeFieldHelp entry

            Nothing => 
              Left (Failure ("Expecting an OBJECT with a field named `" ++ fieldName ++ "`") jsonValue)
           
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
