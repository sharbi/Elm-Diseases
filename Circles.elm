module Circles where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Color as C exposing (..)
import Signal as S exposing (..)
import Window as W
import Mouse as M
import Maybe
import Time exposing (..)
import Keyboard as K exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Debug
import Array as A


type alias ID = Int

type alias NodeModel =
  { title : String
  , id : ID
  , location : (Int, Int)
  , nodeType : Node
  }

type alias Model =
  { nodes : A.Array (NodeModel)
  , editDisease : Bool
  , editSymptoms : Bool
  , name : String
  , id : ID
  , links : List (ID, ID)
  }

nodeInit : ID -> (Int, Int) -> Node -> NodeModel
nodeInit id loc nodeType =
  { title = ""
  , id = id
  , location = loc
  , nodeType = nodeType
  }

initialModel = 
  { nodes = A.empty
  , editDisease = False
  , editSymptoms = False
  , name = ""
  , id = 0
  , links = []
  }

type Node
  = Disease
  | Symptom
  | Empty

type Action
  = NoOp
  | Create Node (Int, Int)
  | Edit
  | Remove ID

actions : Mailbox Action
actions = S.mailbox NoOp


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
    , ("top", ((toString (x - 50)) ++ "px"))
    , ("left", ((toString (y - 75)) ++ "px"))
    , ("font-size", "19px")
    , ("font-weight", "bold")
    , ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)")
    ]

update : Action -> Model -> Model
update action m =
  case action of
    NoOp -> m

    {--Update str id ->
      let newTitle = if (length m.titles < (id - 1)) then A.push str m.titles else A.set id str m.titles
      in 
        { m | titles <- newTitle } -}

    Create nodeType loc ->
      let newModel nodeType = { m | 
                        id <- m.id + 1,
                        nodes <- (A.push (nodeInit m.id loc nodeType) m.nodes),
                        editDisease <- not m.editDisease,
                        editSymptoms <- not m.editSymptoms
                      }
      in
        case nodeType of
          Disease -> newModel nodeType

          Symptom -> newModel nodeType

          Empty -> m
        
       
    {--Update node id shapeAction ->
      let updateNode (nodeID, nodeModel) =
            if nodeID == id then (nodeID,  { nodeModel | shape <- ShapeLayout.update shapeAction diseaseModel.shape } ) else (diseaseID, diseaseModel)

          in
            { m | diseases <- List.map updateDisease m.diseases }

        Symptom ->
          let updateSymptom (symptomID, symptomModel) =
            if symptomID == id then (symptomID, { symptomModel | shape <- ShapeLayout.update shapeAction symptomModel.shape}) else (symptomID, symptomModel)
          in { m | symptoms <- List.map updateSymptom m.symptoms } -}
      

    Edit ->
      { m | editDisease <- not m.editDisease }

  {--  Remove id ->
      let
          newArray array = A.fromList (List.filter (\(nodeID, _) -> nodeID /= id) (A.toIndexedList array))
      in { m | nodes <- newArray m.nodes,
               nodeLocations <- newArray m.nodeLocations,
               titles <- newArray m.titles
        } -}

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
view address m input =
  body [class "disease-map-wrapper", Html.Attributes.style [("position", "absolute")]]
  [ section
    [ id "disease-map-app" ]
    [ (lazy3 buttonBar address (Debug.watch "model" m) input)
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
      nodeType = if m.editDisease then Disease else if m.editSymptoms then Symptom else Empty
  in div [ onClick address (Create nodeType loc), Html.Attributes.style drawStyle] (A.toList (A.map viewNode m.nodes))

viewNode : NodeModel -> Html
viewNode nodeModel =
  let layout color (x, y) =
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
    ]
  in
    case nodeModel.nodeType of
      Disease -> div [style (layout "#9c27b0" nodeModel.location) ] []

      Symptom -> div [style (layout "#D32F2F" nodeModel.location) ] []

linePosition : (Int, Int) -> (Float, Float)
linePosition (x, y) =
  (toFloat y - 450, toFloat x)

{-- 

viewDisease : Address Action -> (ID, D.Model) -> Html
viewDisease address (id, model) =
  let context =
    ShapeLayout.Context (S.forwardTo address (Update Node id)) (S.forwardTo address (always (Remove id)))
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

link : ID -> ID -> Dict ID (Int, Int) -> Dict ID (Int, Int) -> Element
link dId sId dLocs sLocs =
  collage 1000 1000 [traced {defaultLine | width <- 5} (segment (linePosition dLocs dId) (linePosition sLocs sId))] -}


buttonStyle : Bool -> Bool -> List (String, String)
buttonStyle bool1 bool2 =
  let color =
    if bool1 then "#9c27b0" else if bool2 then "#D32F2F" else "#2196F3"
  in [ ("background-color", color), ("display", "inline"), ("color", "#fff"), ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)"), ("height", "30px"), ("margin", "0 auto")]

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

