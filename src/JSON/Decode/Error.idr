module JSON.Decode.Error

import Data.List
import Data.String
import Data.String.Extra
import Language.JSON

%default total

indexedMap : {default 0 idx: Nat} -> (func : (a, Nat) -> b) -> (input: List a) -> List b
indexedMap {idx} func [] = []
indexedMap {idx} func (x::xs) = func (x, idx) :: indexedMap {idx=idx+1} func xs

public export
data Error 
  = Field String Error
  | Index Nat Error
  | OneOf (List Error)
  | Failure String JSON
  | InvalidJSONString String

indent : String -> String
indent str = indentLines 2 str

public export
Show Error where
  show err = assert_total showHelp err []
    where
      showHelp : Error -> List String -> String
      showHelp (Field fieldName err) context =
        let 
          fieldAccess = 
            if isSimple fieldName then
              "." ++ fieldName
            else
              "[\"" ++ fieldName ++ "\"]"
        in 
          showHelp err (fieldAccess::context)
        where
          isSimple : String -> Bool
          isSimple fieldName with (strM fieldName)
            isSimple ""                   | StrNil = False
            isSimple (prim__strCons char rest) | (StrCons char rest) = 
              isAlpha char && all isAlphaNum (unpack rest)
      showHelp (Index idx err) context = 
        showHelp err (("[" ++ show idx ++ "]")::context)
      showHelp (OneOf errors) context = 
        case errors of
          [] => 
            "Ran into a JSON.Decode.oneOf with no possibilities" ++
              case context of
                [] =>
                  "!"

                context => 
                  " at json" ++ concat (reverse context)

          [err] =>
            showHelp err context

          errors =>
            let
              starter =
                case context of
                  [] =>
                    "Json.Decode.oneOf"
                  context =>
                    "The Json.Decode.oneOf at json" ++ concat (reverse context)

              introduction =
                starter ++ " failed in the following " ++ show (length errors) ++ " ways:"
            in 
              join "\n\n" (introduction :: (indexedMap errorOneOf errors))
            where
              errorOneOf : (Error, Nat) -> String
              errorOneOf (error, idx) =
                "\n\n(" ++ show (idx + 1) ++ ") " ++ indent (show error)

      showHelp (Failure msg jsonValue) context = 
        let
          introduction =
            case context of
              [] =>
                "Problem with the given value:\n\n"
              _ =>
                "Problem with the value at json" ++ concat (reverse context) ++ ":\n\n    "
        in
          introduction ++ indent (show jsonValue) ++ "\n\n" ++ msg
      showHelp (InvalidJSONString jsonString) context = "This is not valid JSON! " ++ jsonString