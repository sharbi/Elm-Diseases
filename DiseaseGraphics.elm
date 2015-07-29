module DiseaseGraphics (Model, Action, Context, view, init, update) where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input.Field exposing(..)
import Signal as S exposing (..)
import Text as T
import Color as C

type alias Model =
  {
    title : String
  , id : Int
  , shape : Element
  , location : (Int, Int)
  }

type alias ID = Int

init : ID -> (Int, Int) -> Model
init id loc =
  {
    title = ""
  , id = id
  , shape = collage 4000 2000 [ circle 75 |> filled C.green]
  , location = loc
  }

type Action
  = Update String

type alias Context =
  { actions : S.Address Action }

update : Action -> Model -> Model
update action m =
  case action of
    Update str ->
      { m | title <- str }

view : Address Action -> Model -> Content -> Element
view address m c=
  collage 4000 2000 [ toForm (nameField c), toForm m.shape]

name : Signal.Mailbox Content
name = S.mailbox noContent

nameField : Content -> Element
nameField c =
  field fieldStyle (S.message name.address) "Enter disease name..." c

fieldStyle : Graphics.Input.Field.Style
fieldStyle = 
  { padding = uniformly 0
  , outline = noOutline
  , highlight = noHighlight
  , style = T.defaultStyle
  }

