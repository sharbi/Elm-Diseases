module Symptom where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)
import Signal exposing (..)
import ShapeLayout

type alias Model =
  {
    shape : ShapeLayout.Model
  }

init : Int -> String -> List (String, String) -> Model
init id placeholder style =
  { shape = ShapeLayout.init id placeholder style }

symptomLayout : (Int, Int) -> List (String, String)
symptomLayout loc =
  let v = (fst loc) - 50
      h = (snd loc) - 75
  in [ ("border-radius", "1000px")
    , ("width", "150px")
    , ("border", "0")
    , ("text-align", "center")
    , ("line-height", "100px")
    , ("color", "#fff")
    , ("height", "150px")
    , ("cursor", "pointer")
    , ("background-color", "#E80000")
    , ("display", "inline")
    , ("position", "absolute")
    , ("top", ((toString h) ++ "px"))
    , ("left", ((toString v) ++ "px"))
    , ("font-size", "19px")
    , ("font-weight", "bold")
    , ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)")
    ]

view : ShapeLayout.Context -> Model -> Html
view context m =
  ShapeLayout.view context m.shape

