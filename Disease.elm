module Disease (Model, init, view, defaultLayout, update, Action) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String exposing (..)
import Signal as S exposing (..)
import Symptom exposing (..)
import ShapeLayout exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color as C

type alias ID = Int


type alias Model = 
  { shape : ShapeLayout.Model
  , symptoms : List (ID, Symptom.Model)
  , locations : List (ID, (Int, Int))
  , editSymptoms : Bool
  , uid : ID
  }


init : Int -> String -> List (String, String) -> Model
init id placeholder style = 
  { shape = (ShapeLayout.init id placeholder style)
  , symptoms = []
  , locations = []
  , editSymptoms = False
  , uid = 0
  }


defaultLayout : (Int, Int) -> List(String, String)
defaultLayout loc =
  let v = (fst loc) - 50
      h = (snd loc) - 75
  in
    [ ("border-radius", "1000px")
    , ("width", "150px")
    , ("border", "0")
    , ("text-align", "center")
    , ("line-height", "100px")
    , ("color", "#fff")
    , ("height", "150px")
    , ("cursor", "pointer")
    , ("background-color", "#9400D3")
    , ("display", "inline")
    , ("position", "absolute")
    , ("top", ((toString h) ++ "px"))
    , ("left", ((toString v) ++ "px"))
    , ("font-size", "19px")
    , ("font-weight", "bold")
    , ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)")
    ]

type alias Context =
  { actions : Signal.Address Action }

type Action
  = AddSymptoms (Int, Int)
  | SetBool

update : Action -> Model -> Model
update action m =
  case action of
    AddSymptoms loc ->
      if m.editSymptoms then
        { m |
              uid <- m.uid + 1
            , symptoms <- (m.uid, (Symptom.init m.uid "Symptom name..." (Symptom.symptomLayout loc))) :: m.symptoms
            , locations <- (m.uid, loc) :: m.locations
            , editSymptoms <- not m.editSymptoms
        }
      else m

    SetBool ->
      { m | editSymptoms <- not m.editSymptoms }

view : ShapeLayout.Context -> Model -> Html
view shapeContext m =
  ShapeLayout.view shapeContext m.shape
    
