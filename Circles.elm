module Circles where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color as C exposing (..)
import Signal as S exposing (..)
import Window as W
import Mouse as M
import Maybe
import Time exposing (..)
import Text exposing (..)
import Html.Lazy exposing (..)
import Disease as D exposing (..)
import Dict exposing (..)
import String exposing (..)
import Keyboard as K exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import ShapeLayout exposing (..)
import Symptom exposing (..)

type alias ID = Int

type alias Model =
  { diseases : List (ID, D.Model)
  , symptoms : List (ID, Symptom.Model)
  , editDisease : Bool
  , editSymptoms : Bool
  , name : String
  , dId : ID
  , sId : ID
  , diseaseLocations : Dict ID (Int, Int)
  , symptomLocations : Dict ID (Int, Int)
  , lines : List (Element)
  }

initialModel = 
  { diseases = []
  , symptoms = []
  , editDisease = False
  , editSymptoms = False
  , name = ""
  , dId = 0
  , sId = 0
  , diseaseLocations = Dict.empty
  , symptomLocations = Dict.empty
  , lines = []
  }

type Action
  = NoOp
  | UpdateTitle String
  | Update Shape ID ShapeLayout.Action
  | Create Shape (Int, Int)
  | AddSymptoms (Int, Int)
  | Edit
  | Remove Shape ID

type Shape
  = Disease
  | Symptom

actions : Mailbox Action
actions = S.mailbox NoOp

update : Action -> Model -> Model
update action m =
  case action of
    NoOp -> m

    UpdateTitle str ->
      { m | name <- str }

    Create shape loc ->
      case shape of
        Disease ->
          if m.editDisease then
            { m | 
              dId <- m.dId + 1,
              name <- "",
              diseases <- (m.dId, (D.init m.dId "Disease Name..." (D.defaultLayout loc))) :: m.diseases,
              diseaseLocations <- insert m.dId loc m.diseaseLocations,
              editDisease <- not m.editDisease,
              editSymptoms <- not m.editSymptoms
            }

          else m
        Symptom ->
          if m.editSymptoms then
            { m | 
                sId <- m.sId + 1,
                name <- "",
                symptoms <- (m.sId, (Symptom.init m.sId "Symptom Name..." (Symptom.symptomLayout loc))) :: m.symptoms,
                symptomLocations <- insert m.sId loc m.symptomLocations,
                lines <- (addLines m.dId m.sId m.diseaseLocations m.symptomLocations) :: m.lines
            }

          else m

    Update shape id shapeAction ->
      case shape of 
        Disease ->
          let updateDisease (diseaseID, diseaseModel) =
                if diseaseID == id then (diseaseID,  { diseaseModel | shape <- ShapeLayout.update shapeAction diseaseModel.shape } ) else (diseaseID, diseaseModel)

          in
            { m | diseases <- List.map updateDisease m.diseases }

        Symptom ->
          let updateSymptom (symptomID, symptomModel) =
            if symptomID == id then (symptomID, { symptomModel | shape <- ShapeLayout.update shapeAction symptomModel.shape}) else (symptomID, symptomModel)
          in { m | symptoms <- List.map updateSymptom m.symptoms }
      

    Edit ->
      { m | editDisease <- not m.editDisease }

    Remove shape id ->
      case shape of
        Disease ->
          { m | diseases <- List.filter (\(diseaseID, _) -> diseaseID /= id) m.diseases }

        Symptom ->
          {m | symptoms <- List.filter (\(symptomID, _) -> symptomID /= id) m.symptoms }

type alias Input = {
    point : (Int, Int)
  }

drawStyle : List(String, String)
drawStyle =
  [ ("height", "1000px")
  , ("width", "1000px")
  , ("position", "relative")
  ]

view : Address Action -> Model -> Input -> Html
view address m input=
  body [class "disease-map-wrapper", Html.Attributes.style [("position", "absolute")]]
  [ section
    [ id "disease-map-app" ]
    [ (lazy3 buttonBar address m input)
    , (lazy3 combinedSpace address m input)
    ] 
  ]   


buttonBar : Address Action -> Model -> Input -> Html
buttonBar address m input =
  let editButton = Html.button [ (Html.Attributes.style (buttonStyle m.editDisease m.editSymptoms)), onClick address Edit ] [ Html.text "Create New Disease" ]
  in
     Html.header [ id "header" ]
    [ section []
      [ editButton
      , fromElement (show input.point)
      ]
    ]

stylesForShapes : List (String, String)
stylesForShapes =
  [ ("height", "inherit")
  , ("width", "inherit")
  , ("position", "absolute")
  , ("z-index", "1")
  ]

styleForLine : List (String, String)
styleForLine =
  [ ("height", "inherit")
  , ("width", "inherit")
  , ("z-index", "-2")
  ]

combinedSpace : Address Action -> Model -> Input -> Html
combinedSpace address m input =
  let loc = input.point
      action = if m.editDisease then (Create Disease loc) else if m.editSymptoms then (Create Symptom loc) else NoOp
  in div [ onClick address action, Html.Attributes.style drawStyle] [(diseaseSpace address m input), (symptomSpace address m input), (lineSpace address m)]

diseaseSpace : Address Action -> Model -> Input -> Html
diseaseSpace address m input=
  let loc = input.point
  in div [ Html.Attributes.style stylesForShapes ] (List.map (viewDisease address) m.diseases)

lineSpace : Address Action -> Model -> Html
lineSpace address m =
  div [Html.Attributes.style styleForLine ] (List.map drawLines m.lines)

viewDisease : Address Action -> (ID, D.Model) -> Html
viewDisease address (id, model) =
  let context =
    ShapeLayout.Context (S.forwardTo address (Update Disease id)) (S.forwardTo address (always (Remove Disease id)))
  in D.view context model

symptomSpace : Address Action -> Model -> Input -> Html
symptomSpace address m input =
  let loc = input.point
  in div [Html.Attributes.style stylesForShapes] (List.map (viewSymptom address) m.symptoms)

viewSymptom : Address Action -> (ID, Symptom.Model) -> Html
viewSymptom address (id, model) =
  let context =
    ShapeLayout.Context (S.forwardTo address (Update Symptom id)) (S.forwardTo address (always (Remove Symptom id)))
  in Symptom.view context model

addLines : ID -> ID -> Dict ID (Int, Int) -> Dict ID (Int, Int) -> Element
addLines dId sId dLocs sLocs =
  let dLoc = Maybe.withDefault (0, 0) (get dId dLocs)
      sLoc = Maybe.withDefault (0, 0) (get sId sLocs)
      a = (Basics.toFloat (fst dLoc))
      b = (Basics.toFloat (snd dLoc))
      c = (Basics.toFloat (fst sLoc))
      d = (Basics.toFloat (snd sLoc))
  in collage 1000 1000 [traced {defaultLine | width <- 5} (segment ((a - 500), (450 - b)) ((c - 500), (450 - d)))] 


drawLines : Element -> Html
drawLines line =
  fromElement line

buttonStyle : Bool -> Bool -> List (String, String)
buttonStyle bool1 bool2 =
  let color =
    if bool1 then "#9c27b0" else if bool2 then "#D32F2F" else "#2196F3"
  in [ ("background-color", color), ("display", "inline"), ("color", "#fff"), ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)"), ("height", "30px"), ("margin", "0 auto")]


main : Signal Html
main = view actions.address <~ model ~ userInput

delta : Signal Time
delta = S.map (\t -> t / 1500) (fps 10)

userInput : Signal Input
userInput = 
  S.sampleOn delta <|
    S.map Input M.position

model : Signal Model
model = S.foldp update initialModel actions.signal

