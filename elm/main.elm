import Signal
import Signal((<~), (~))
import Window
import Result
import Graphics.Element (Element)
import String

import Html(toElement)
import Dict

import Types(..)
import JSON
import View

import Debug

-- port in
port initialHash : String
port databaseStr : String
port once        : Signal.Signal Bool
port getHash     : Signal.Signal String

-- port out
port setHash : Signal.Signal String
port setHash = Signal.map (.page >> pageToHash) state

port title : Signal.Signal String
port title = Signal.map (.page >> pageToTitle) state

port scroll      : Signal.Signal Int
port scroll = Signal.map (\_ -> 0) (Signal.subscribe actions)

-- page converter
hashToPage : String -> Page
hashToPage h = case String.split "/" h of
  ["#", "packages"]        -> AllPackagesPage
  ["#", "package", u, p]   -> PackagePage (u ++ "/" ++ p)
  ["#", "module", u, p, m] -> ModulePage (u ++ "/" ++ p) m
  _ -> TopPage

pageToHash : Page -> String
pageToHash p = case p of
  PackagePage p   -> "#/package/" ++ p
  AllPackagesPage -> "#/packages"
  ModulePage p m  -> "#/module/" ++ p ++ "/" ++ m
  _               -> ""

pageToTitle : Page -> String
pageToTitle p = case p of
  PackagePage p   -> "package: " ++ p
  AllPackagesPage -> "All Packages"
  ModulePage p m  -> "module: " ++ m ++ " " ++ p
  _               -> "top"

initialState : State
initialState = State (hashToPage initialHash) Dict.empty Nothing

state : Signal.Signal State
state = Signal.foldp step initialState <| Signal.mergeMany
  [ Signal.subscribe actions
  , Signal.map HashChanged getHash
  , database
  ]

step : Action -> State -> State
step action state = case action of
  NoOp                ->  state
  FocusPage page      -> {state | page <- page }
  HashChanged     h   -> {state | page <- hashToPage h }
  SetDatabase     db  -> {state | database <- db, error <- Nothing }
  SetError        e   -> {state | error <- Just e}

database : Signal.Signal Action
database = Signal.sampleOn once <| Signal.constant <| case JSON.parse databaseStr of
  Result.Ok  d -> SetDatabase d
  Result.Err e -> SetError e

main : Signal.Signal Element
main = scene <~ state ~ Window.dimensions

scene : State -> (Int, Int) -> Element
scene state (w,h) = toElement w h (View.view state)
