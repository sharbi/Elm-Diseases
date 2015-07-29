module Circles where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color as C exposing (..)
import Signal as S exposing (..)
import Window as W
import Mouse as M
import Time exposing (..)
import Text exposing (..)
import Html.Lazy exposing (..)
import Disease as D exposing (..)
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
  , dId : Int
  , sId : Int
  , diseaseLocations : List (ID, (Int, Int))
  , symptomLocations : List (ID, (Int, Int))
  , lines : List (Element)
  , allLines : List (Element)
  }

initialModel = 
  { diseases = []
  , symptoms = []
  , editDisease = False
  , editSymptoms = False
  , name = ""
  , dId = 0
  , sId = 0
  , diseaseLocations = []
  , symptomLocations = []
  , lines = []
  , allLines = []
  }

type Action
  = NoOp
  | UpdateTitle String
  | Update String ID ShapeLayout.Action
  | Create String (Int, Int)
  | AddSymptoms (Int, Int)
  | Edit
  | Remove String ID

actions : Mailbox Action
actions = S.mailbox NoOp

update : Action -> Model -> Model
update action m =
  case action of
    NoOp -> m

    UpdateTitle str ->
      { m | name <- str }

    Create list loc ->
      case list of
        "disease" ->
          if m.editDisease then
            { m | 
              dId <- m.dId + 1,
              name <- "",
              diseases <- (m.dId, (D.init m.dId "Disease Name..." (D.defaultLayout loc))) :: m.diseases,
              diseaseLocations <- (m.dId, loc) :: m.diseaseLocations,
              editDisease <- not m.editDisease,
              editSymptoms <- not m.editSymptoms
            }

          else m
        "symptom" ->
          if m.editSymptoms then
            let loc1 id = List.map snd (List.filter (\(diseaseID, _) -> diseaseID == id) m.diseaseLocations)
            in  { m | 
                  sId <- m.sId + 1,
                  name <- "",
                  symptoms <- (m.sId, (Symptom.init m.sId "Symptom Name..." (Symptom.symptomLayout loc))) :: m.symptoms,
                  symptomLocations <- (m.sId, loc) :: m.symptomLocations,
                  lines <- (List.map (addLines loc) (loc1 (m.dId - 1))),
                  allLines <- lines :: m.allLines
                }

          else m

    Update list id shapeAction ->
      case list of 
        "disease" ->
          let updateDisease (diseaseID, diseaseModel) =
                if diseaseID == id then (diseaseID,  { diseaseModel | shape <- ShapeLayout.update shapeAction diseaseModel.shape } ) else (diseaseID, diseaseModel)

          in
            { m | diseases <- List.map updateDisease m.diseases }

        "symptom" ->
          let updateSymptom (symptomID, symptomModel) =
            if symptomID == id then (symptomID, { symptomModel | shape <- ShapeLayout.update shapeAction symptomModel.shape}) else (symptomID, symptomModel)
          in { m | symptoms <- List.map updateSymptom m.symptoms }
      

    Edit ->
      { m | editDisease <- not m.editDisease }

    Remove list id ->
      case list of
        "disease" ->
          { m | diseases <- List.filter (\(diseaseID, _) -> diseaseID /= id) m.diseases }

        "symptom" ->
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
      action = if m.editDisease then (Create "disease" loc) else if m.editSymptoms then (Create "symptom" loc) else NoOp
  in div [ onClick address action, Html.Attributes.style drawStyle] [(diseaseSpace address m input), (symptomSpace address m input), (lineSpace address m)]

diseaseSpace : Address Action -> Model -> Input -> Html
diseaseSpace address m input=
  let loc = input.point
  in div [ Html.Attributes.style stylesForShapes ] (List.map (viewDisease address) m.diseases)

lineSpace : Address Action -> Model -> Html
lineSpace address m =
  div [Html.Attributes.style styleForLine ] (List.map drawLines m.allLines)

viewDisease : Address Action -> (ID, D.Model) -> Html
viewDisease address (id, model) =
  let context =
    ShapeLayout.Context (S.forwardTo address (Update "disease" id)) (S.forwardTo address (always (Remove "disease" id)))
  in D.view context model

symptomSpace : Address Action -> Model -> Input -> Html
symptomSpace address m input =
  let loc = input.point
  in div [Html.Attributes.style stylesForShapes] (List.map (viewSymptom address) m.symptoms)

viewSymptom : Address Action -> (ID, Symptom.Model) -> Html
viewSymptom address (id, model) =
  let context =
    ShapeLayout.Context (S.forwardTo address (Update "symptom" id)) (S.forwardTo address (always (Remove "symptom" id)))
  in Symptom.view context model

addLines : (Int, Int) -> (Int, Int) ->  Element
addLines loc1 loc2 =
  let a = Basics.toFloat (fst loc1) - 475
      b = 500 - Basics.toFloat (snd loc1)
      x = Basics.toFloat (fst loc2) - 475
      y = 500 - Basics.toFloat (snd loc2)
  in collage 1000 1000 [traced {defaultLine | width <- 5} (segment (a, b) (x, y))] 


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

