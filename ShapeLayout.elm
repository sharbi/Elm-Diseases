module ShapeLayout (Model, Action, init, update, Context, view) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal as S exposing (..)

type alias ID = Int

type alias Model =
  { title : String
  , id : ID
  , style : List (String, String)
  , placeholder : String
  }

init : ID -> String -> List (String, String) -> Model
init id placeholder style =
  { title = ""
  , id = id
  , style = style
  , placeholder = placeholder
  }


type Action
  = Update String

update : Action -> Model -> Model
update action m =
  case action of
    Update str ->
      { m | title <- str }

type alias Context =
  { actions : Signal.Address Action
  , remove : Signal.Address ()
  }

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

view : Context -> Model -> Html
view context m =
  section [style m.style]
    [ input
        [ placeholder m.placeholder
        , autofocus True
        , value m.title
        , on "input" targetValue (\str -> S.message context.actions (Update str))
        , style inputLayout
        ]
      []
    , button [onClick context.remove (), style removeButton] [Html.text "X"]
    ]
