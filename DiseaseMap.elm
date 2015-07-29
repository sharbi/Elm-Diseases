module DiseaseMap where

import Color exposing (..)
import Mouse as M
import Window as W
import Disease as D
import Signal
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input as Input
import StartApp
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type alias Model = {
  numberOfDiseases : Int
  , locations : List(Int, Int)
  , editMode : Bool
}

initialModel = { numberOfDiseases = 0, locations = [], editMode = False}

main = 
  StartApp.start{ model = initialModel, update = update, view = scene }

clickLocations : Signal (List (Int, Int))
clickLocations =
  Signal.foldp (::) [] (Signal.sampleOn M.clicks M.position)

type Action =
  Edit Bool
  | ClickCreate

update : Action -> Model -> Model
update action model =
  case action of
    Edit bool ->
      { model | editMode <- bool }
    ClickCreate ->
      { model | numberOfDiseases <- model.numberOfDiseases + 1 }
      

newDisease : (Int, Int) -> (Int, Int) -> Element
newDisease (w, h) (x, y) =
  let disease (x, y) =
    [ D.init
      |> move (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y) ]
  in collage w h (disease (x, y))

diseases : Signal List(Int, Int) -> Signal (List(Element))
diseases locs =
  Signal.foldp (::) [] (Signal.map2 newDisease W.dimensions locs)

{--if model.editMode then
        Just (Signal.map fromElement (Signal.map2 makeDisease model.size model.locs))
      else Nothing -}

scene : Signal.Address Action -> Model -> Html
scene address model =
  div [] <|
    []
    ++ (checkbox address model.editMode Edit "Create a Disease")

checkbox : Signal.Address Action -> Bool -> (Bool -> Action) -> String -> List Html
checkbox address isChecked tag name =
  [ input 
    [ type' "checkbox"
    , checked isChecked
    , on "change" targetChecked (Signal.message address << tag)
    ]
    []
  , Html.text name
  , br [] []
  ]