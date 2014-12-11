module Types where

import Dict
import Signal

type alias Database = Dict.Dict String Package

type alias Package =
  { summary : String
  , modules : Dict.Dict String Module
  }

type alias Module =
  { package  : String
  , comment  : List Comment
  , aliases  : Dict.Dict String AliasDef
  , types    : Dict.Dict String TypeDef
  , values   : Dict.Dict String ValueDef
  }

type Comment
  = Header Int String
  | Docs  (List String)
  | Plain String

type alias AliasDef =
  { comment : String
  , args    : List String
  , typeSig : Type
  }

type alias TypeDef =
  { comment : String
  , args    : List String
  , cases   : List (String, List Type)
  }

type alias ValueDef =
  { comment : String
  , typeSig : Type
  }

type Type
  = Type String
  | Var  String
  | App Type (List Type)
  | Lambda Type Type
  | Record (List (String, Type))

-- state
type alias State =
  { page     : Page
  , database : Database
  , error    : Maybe String
  }

type Page
  = TopPage
  | PackagePage String
  | AllPackagesPage
  | ModulePage  String String

-- action
type Action
  = NoOp
  | FocusPage    Page
  | HashChanged  String
  | SetDatabase  Database
  | SetError     String

-- action cnannel
actions : Signal.Channel Action
actions = Signal.channel NoOp
