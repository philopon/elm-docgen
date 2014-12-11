module JSON (parse) where

import Json.Decode((:=))
import Json.Decode as J
import Dict
import Result

import Types(..)

-- parse json
jPackages : J.Decoder Database
jPackages = J.map Dict.fromList <| J.list (("package" := J.string) `J.andThen` \p -> J.object1 ((,) p) (jPackage p))

jPackage : String -> J.Decoder Package
jPackage pkg = J.object2 Package
  ("summary" := J.string)
  ("modules" := J.map Dict.fromList (J.list (J.object2 (,) ("name" := J.string) (jModule pkg))))

jModule : String -> J.Decoder Module
jModule pkg = J.object4 (Module pkg)
  ("comment"  := J.list jComment)
  ("aliases"  := J.map Dict.fromList (J.list (J.object2 (,) ("name" := J.string) jAliasDef)))
  ("types"    := J.map Dict.fromList (J.list (J.object2 (,) ("name" := J.string) jTypeDef)))
  ("values"   := J.map Dict.fromList (J.list (J.object2 (,) ("name" := J.string) jValueDef)))

jComment' : String -> J.Decoder Comment
jComment' t = case t of
  "header" -> J.object2 Header ("level" := J.int) ("body" := J.string)
  "docs"   -> J.object1 Docs  ("docs" := J.list J.string)
  "plain"  -> J.object1 Plain ("body" := J.string)

jComment : J.Decoder Comment
jComment = ("tag" := J.string) `J.andThen` jComment'

jAliasDef : J.Decoder AliasDef
jAliasDef = J.object3 AliasDef
  ("comment" := J.string)
  ("args"    := J.list J.string)
  ("type"    := jType)

jTypeDef : J.Decoder TypeDef
jTypeDef = J.object3 TypeDef
  ("comment" := J.string)
  ("args"    := J.list J.string)
  ("cases"   := J.list (J.tuple2 (,) J.string (J.list jType)))

jValueDef : J.Decoder ValueDef
jValueDef = J.object2 ValueDef
  ("comment" := J.string)
  ("type"    := jType)

jType' : String -> J.Decoder Type
jType' tag = case tag of
  "type"   -> J.object1 Type   ("name" := J.string)
  "var"    -> J.object1 Var    ("name" := J.string)
  "app"    -> J.object2 App    ("func" := jType) ("args" := J.list jType)
  "lambda" -> J.object2 Lambda ("in" := jType) ("out" := jType)
  "record" -> J.object1 Record ("fields" := (J.list (J.tuple2 (,) J.string jType )))
  _        -> J.fail (tag ++ " is not a recognized tag")

jType : J.Decoder Type
jType = "tag" := J.string `J.andThen` jType'

parse : String -> Result.Result String Database
parse = J.decodeString jPackages
