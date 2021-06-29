module JSON.Encode

import Data.Vect

import public Language.JSON.Data

||| Convert `JSON` into a prettified string.
|||
||| ```idris example
||| > :let tom : JSON
||| > :let tom =
|||     JSON.Encode.object
|||       [ ( "name", JSON.Encode.string "Tom" )
|||       , ( "age", JSON.Encode.int 42 )
|||       ]
|||
||| > encode tom
||| "{\"name\":\"Tom\",\"age\":42}"
|||
||| > encode {intentation=4} tom
||| "{
|||      \"name\": \"Tom\",
|||      \"age\": 42
|||  }"
||| ```idris example
export
encode : {default 0 indentation: Nat} -> JSON -> String
encode {indentation} jsonValue =
  format indentation jsonValue

--------------------------------------------------------------------------------
-- JSON primitives
--------------------------------------------------------------------------------

||| Turn a `String` into a JSON string.
|||
||| ```idris example
||| > encode (string "")
||| """"
||| ```
||| ```idris example
||| > encode (string "abc")
||| ""abc""
||| ```
public export
string : String -> JSON
string value = JString value

||| Turn an `Int` into a JSON number.
|||
||| ```idris example
||| > encode (int 42)
||| "42"
||| ```
||| ```idris example
||| > encode (int (-7))
||| "-7"
||| ```
public export
int : Int -> JSON
int value = JNumber (cast value)

||| Turn a `Double` into a JSON number.
|||
||| ```idris example
||| > encode (double 3.14)
||| "3.14"
||| ```
||| ```idris example
||| > encode (double (-42))
||| "-42"
||| ```
public export
double : Double -> JSON
double value = JNumber value

||| Turn a `Bool` into a JSON boolean.
||| 
||| ```idris example
||| > encode (bool True)
||| "true"
||| ```
||| ```idris example
||| > encode (bool False)
||| "false"
||| ```
public export
bool : Bool -> JSON
bool value = JBoolean value

||| Create a JSON `null` value.
|||
||| ```idris example
||| > encode null
||| "null"
||| ```
null : JSON
null = JNull

--------------------------------------------------------------------------------
-- Arrays
--------------------------------------------------------------------------------

||| Turn a `List` into a JSON array.
|||
||| ```idris example
||| > encode (list int [1,3,4])
||| "[1,3,4]"
||| ```
||| ```idris example
||| > encode (list bool [True,False])
||| "[true,false]"
||| ```
public export
list : (a -> JSON) -> List a -> JSON
list f xs = JArray (map f xs)

||| Turn a `Vect` into a JSON array.
|||
||| ```idris example
||| > encode (vect int [1,3,4])
||| "[1,3,4]"
||| ```
||| ```idris example
||| > encode (vect bool [True,False])
||| "[true,false]"
||| ```
public export
vect : (a -> JSON) -> Vect len a -> JSON
vect f xs = list f (toList xs)

--------------------------------------------------------------------------------
-- JSON objects
--------------------------------------------------------------------------------

||| Create a JSON object.
|||
||| ```idris example
||| > encode (object [ ( "name", Encode.string "Tom" ) , ( "age", Encode.int 42 ) ])
||| "{"name":"Tom","age":42}"
||| ```
public export
object : List (String, JSON) -> JSON
object pairs = JObject pairs