module View(view) where

import Html
import Html.Attributes as A
import Html.Events(onClick, onKeyPress)

import Signal

import List
import List((::))
import String
import Maybe
import Dict
import Types(..)

import Debug

onPressEnter : Signal.Message -> Html.Attribute
onPressEnter msg = onKeyPress (\key -> if key == 13 then msg else Signal.send actions NoOp)

-- view
view state = Html.div [A.class "container"] [navBar state, content state]

navBar : State -> Html.Html
navBar state =
  let msg = Signal.send actions (FocusPage TopPage)
  in Html.nav [A.class "navbar"] [Html.ul []
     [ Html.li [A.class "brand", onClick msg, onPressEnter msg] [Html.text "Elm-doc"]
     ]]

content : State -> Html.Html
content state = case state.error of
  Just err -> errorView err state
  Nothing  -> case state.page of
    TopPage            -> topPageView state
    AllPackagesPage    -> allPackagesView state
    PackagePage    pkg -> packagePageView pkg state
    ModulePage pkg mdl -> modulePageView pkg mdl state

errorView : String -> State -> Html.Html
errorView err state = Html.node "main" [A.class "error"] [Html.h1 [] [Html.text "Error!"], Html.p [] [Html.text err]]

-- top page
topPageView : State -> Html.Html
topPageView state = Html.node "main" [A.class "top"] <|
  [ Html.h1 [] [Html.text "Packges"]
  , Html.dl [] <|
      packageItem (FocusPage AllPackagesPage) "All" {summary = "All packages"} ++
      List.concatMap
        (\(n,p) -> packageItem (FocusPage <| PackagePage n) n p)
        (Dict.toList state.database)
  ]

packageItem : Action -> String -> { p | summary:String } -> List Html.Html
packageItem msg name p =
  let sendMsg = Signal.send actions msg
  in [ Html.dt [] [Html.a [onClick sendMsg, onPressEnter sendMsg, A.tabindex 0] [Html.text name]]
     , Html.dd [A.class "comment"] [Html.text p.summary]
     ]

allPackagesView : State -> Html.Html
allPackagesView state =
  let mods = Dict.values state.database |> List.map .modules >> List.foldl Dict.union Dict.empty
      pkg  = Package "All Pakcages" mods
  in modulesView "All Packages" pkg

-- package page
packagePageView : String -> State -> Html.Html
packagePageView pName state = case Dict.get pName state.database of
  Nothing  -> errorView (String.append pName " not found.") state
  Just pkg -> modulesView pName pkg

modulesView : String -> Package -> Html.Html
modulesView pName pkg = Html.node "main" [A.class "package"] <|
  [ Html.h1 [] [Html.text pName]
  , Html.p  [A.class "comment"] [Html.text pkg.summary]
  , Html.h2 [] [Html.text "Modules"]
  , Html.dl [] (List.concatMap (moduleItem pName) (Dict.toList pkg.modules))
  ]

moduleItem : String -> (String, Module) -> List Html.Html
moduleItem pkg (name, mdl) =
  let com = case mdl.comment of
        Plain b::_ -> [Html.dd [A.class "comment"] [Html.text b]]
        _          -> []
      msg = Signal.send actions (FocusPage <| ModulePage mdl.package name)
  in Html.dt [] [Html.a [A.tabindex 0, onClick msg, onPressEnter msg] [Html.text name]] :: com

-- module page
modulePageView : String -> String -> State -> Html.Html
modulePageView pName mName state = case Dict.get pName state.database `Maybe.andThen` (.modules >> Dict.get mName) of
  Nothing  -> errorView (String.concat [pName, " ", mName, " not found."]) state
  Just mdl ->
    let toView a = case a of
          Header 1 b -> Html.h2  [] [Html.text b]
          Header 2 b -> Html.h3  [] [Html.text b]
          Header 3 b -> Html.h4  [] [Html.text b]
          Header 4 b -> Html.h5  [] [Html.text b]
          Header _ b -> Html.h6  [] [Html.text b]
          Docs     b -> Html.dl  [] (List.concat <| List.filterMap (documentItem mdl) b)
          Plain    b -> Html.pre [A.class "description"] [Html.code [] [Html.text b]]
        msg = Signal.send actions (FocusPage <| PackagePage pName)
    in Html.node "main" [A.class "module"] <|
    [ Html.h1 [] [Html.text mName, Html.text " ", Html.small [A.tabindex 0, onClick msg, onPressEnter msg] [Html.text <| pName]]
    ] ++ List.map toView mdl.comment

documentItem : Module -> String -> Maybe (List Html.Html)
documentItem mdl name = Maybe.oneOf
  [ documentValueItem mdl.values name
  , documentTypeItem  mdl.types name
  , documentAliasItem mdl.aliases name
  ]

documentValueItem : Dict.Dict String ValueDef -> String -> Maybe (List Html.Html)
documentValueItem vals item = Dict.get (stripParen item) vals `Maybe.andThen` \k -> Just <|
  [ Html.dt []
    [ Html.span [A.class "name"] [Html.text item]
    , Html.span [A.class "type-colon"] [Html.text " : "]
    , typeSigView k.typeSig
    ]
  , Html.dd [A.class "description"] [Html.pre [] [Html.code [] [Html.text k.comment]]]
  ]

documentTypeItem : Dict.Dict String TypeDef -> String -> Maybe (List Html.Html)
documentTypeItem types item = Dict.get item types `Maybe.andThen` \k -> Just <|
  [ Html.dt [] <|
    [ Html.span [A.class "keyword type"] [Html.text "type "]
    , Html.span [A.class "name"] [Html.text item]
    , Html.span [A.class "type-args"] (List.map (\a -> Html.text (' ' `String.cons` a)) k.args)
    , Html.span [A.class "type-equal"] [Html.text " = "]
    ] ++ List.intersperse (Html.span [A.class "type-pipe"] [Html.text " | "]) (List.map typeCaseItem k.cases)
  , Html.dd [A.class "description"] [Html.pre [] [Html.code [] [Html.text k.comment]]]
  ]

typeCaseItem (name, types) = Html.span [A.class "case"] <|
  Html.span [A.class "type"] [Html.text name] ::
  List.concatMap (\t -> Html.text " " :: [typeSigView t]) types

documentAliasItem : Dict.Dict String AliasDef -> String -> Maybe (List Html.Html)
documentAliasItem aliases item = Dict.get item aliases `Maybe.andThen` \k -> Just <|
  [ Html.dt []
    [ Html.span [A.class "keyword alias "] [Html.text "type alias "]
    , Html.span [A.class "name"] [Html.text item]
    , Html.span [A.class "type-equal"] [Html.text " = "]
    , typeSigView k.typeSig
    ]
  , Html.dd [A.class "description"] [Html.pre [] [Html.code [] [Html.text k.comment]]]
  ]

typeSigView : Type -> Html.Html
typeSigView t = Html.span [A.class "type-signature"] [typeSigView' t]

isLambda : Type -> Bool
isLambda t = case t of
  (Lambda _ _) -> True
  _ -> False

typeSigView' : Type -> Html.Html
typeSigView' sig = case sig of
  Type "_Tuple0" -> Html.span [A.class "app tuple 0-tuple"] [Html.text "()"]
  Type t         -> Html.span [A.class "type"] [Html.text t]
  Var  t         -> Html.span [A.class "var"] [Html.text t]
  App (Type "_Tuple2") a -> Html.span [A.class "app tuple 2-tuple"] (tupleSig a)
  App (Type "_Tuple3") a -> Html.span [A.class "app tuple 3-tuple"] (tupleSig a)
  App (Type "_Tuple4") a -> Html.span [A.class "app tuple 4-tuple"] (tupleSig a)
  App (Type "_Tuple5") a -> Html.span [A.class "app tuple 5-tuple"] (tupleSig a)
  App con vs -> Html.span [A.class "app"] <|
    typeSigView' con ::
    List.concatMap (typeSigView' >> \a -> [Html.span [] [Html.text " "], Html.span [] [a]]) vs
  Lambda a b -> Html.span [A.class "lambda"] <|
    (if isLambda a
      then Html.span [A.class "hi-lambda"]
        [ Html.span [A.class "paren hi-lambda-open"] [Html.text "("]
        , typeSigView' a
        , Html.span [A.class "paren hi-lambda-close"] [Html.text ")"]
        ]
      else typeSigView' a) ::
    Html.span [A.class "arrow"] [Html.text " -> "] ::
    [typeSigView' b]
  Record r   -> Html.span [A.class "record"] <|
    Html.span [A.class "curly curly-open"]  [Html.text "{"] ::
    ( List.intersperse (Html.span [A.class "comma record-comma"] [Html.text ", "]) <|
      List.map (\(k, v) -> Html.span [A.class "field"]
        [ Html.span [A.class "key"] [Html.text k]
        , Html.span [A.class "type-colon"] [Html.text " : "]
        , typeSigView v
        ]) r) ++
    [Html.span [A.class "curly curly-close"] [Html.text "}"]]

--   | Record (List (String, Type))
tupleSig : List Type -> List Html.Html
tupleSig list =
  let comma = Html.span [A.class "comma tuple-comma"] [Html.text ", "]
  in Html.span [A.class "paren tuple-open"] [Html.text "("] ::
     List.intersperse comma (List.map typeSigView' list) ++
     [Html.span [A.class "paren tuple-close"] [Html.text ")"]]

stripParen : String -> String
stripParen a = if String.left 1 a == "(" && String.right 1 a == ")" then String.slice 1 (-1) a else a
