module Node (Model, init, view, update, Context, Action) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array as A
import Signal as S exposing (..)

{-- Type declarations -}

type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

type NodeType
    = Disease
    | Symptom

type Action
    = NoOp
    | Update String
    | ChangeBool

type alias Model =
  { title : String
  , location : (Int, Int)
  , nodeType : NodeType
  , autofocus : Bool
  }

{--Initialise model and update -}

init : (Int, Int) -> Int -> Model
init loc num =
  let nodeType = if (num == 1) then Disease else Symptom
  in  { title = ""
      , location = loc
      , nodeType = nodeType
      , autofocus = True
      }



update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    Update str ->
      { model | title <- str }

    ChangeBool ->
      { model | autofocus <- not model.autofocus }


{--Views and Styles -}

layout : String -> (Int, Int) -> List (String, String)
layout color (x, y) =
    [ ("border-radius", "1000px")
    , ("width", "150px")
    , ("border", "0")
    , ("text-align", "center")
    , ("line-height", "100px")
    , ("color", "#fff")
    , ("height", "150px")
    , ("cursor", "pointer")
    , ("background-color", color)
    , ("display", "inline")
    , ("position", "absolute")
    , ("top", ((toString (y - 125)) ++ "px"))
    , ("left", ((toString (x - 75)) ++ "px"))
    , ("font-size", "19px")
    , ("font-weight", "bold")
    , ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)")
    , ("z-index", "2")
    , ("autofocus", "autofocus")
    ]

view : Context -> Model -> Html
view context model =
  let 
      html place color model =
        div [ style (layout color model.location), onClick context.actions ChangeBool ]
          [ input
              [ placeholder place
              , autofocus model.autofocus
              , value model.title
              , on "input" targetValue (\str -> S.message context.actions (Update str))
              , style inputLayout
              ]
          []
          , button [onClick context.remove (), style removeButton] [Html.text "X"]
          ]
  in
    case model.nodeType of
      Disease -> html "Disease Name..." "#9c27b0" model
      
      Symptom -> html "Symptom Name..." "#D32F2F" model

removeButton : List (String, String)
removeButton =
  [ ("color", "#000")
  , ("height", "12px")
  , ("z-index", "2")
  , ("position", "absolute")
  , ("float", "right")
  , ("top", "0px")
  , ("right", "0px")
  ]

inputLayout : List (String, String)
inputLayout =
  [ ("background-color", "inherit")
  , ("border", "0")
  , ("border-radius", "1000px")
  , ("color", "#fff") 
  , ("type", "hidden")
  , ("width", "inherit")
  , ("text-align", "center")
  , ("font-weight", "bold")
  , ("font-size", "16px")
  , ("line-height", "145px")
  ]