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
import Node as N exposing (..)


type alias ID = Int


type alias Model =
  { nodes : List (ID, N.Model)
  , editDisease : Bool
  , editSymptoms : Bool
  , id : ID
  , links : List (ID, ID)
  , storedDisease : Int
  }



initialModel = 
  { nodes = []
  , editDisease = False
  , editSymptoms = False
  , id = 0
  , links = []
  , storedDisease = 0
  }


type Action
  = NoOp
  | Update ID N.Action
  | Create Int (Int, Int)
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

    Update id nodeAction ->
      let updateNode (nodeId, nodeModel) =
        if nodeId == id then
          (nodeId, N.update nodeAction nodeModel)
        else (nodeId, nodeModel)
      in
        { m | nodes <- List.map updateNode m.nodes}

    Create num loc ->
      let newModel num = (m.id, (N.init loc num)) :: m.nodes
          newLink = (m.storedDisease, m.id) :: m.links
      in
        case num of
          1 -> 
            { m | id <- m.id + 1,
                  nodes <- newModel num,
                  editDisease <- not m.editDisease,
                  editSymptoms <- not m.editSymptoms,
                  storedDisease <- m.id
            }
                
          2 -> 
            { m | id <- m.id + 1,
                  nodes <- newModel num,
                  links <- newLink
            }

          4 -> m

    Remove id ->
      { m | nodes <- List.filter (\(nodeId, _) -> nodeId /= id) m.nodes}
        
       
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

drawStyle : List (String, String)
drawStyle =
  [ ("height", "1000px")
  , ("width", "1000px")
  , ("position", "relative")
  , ("z-index", "1")
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
      ]
    ]

stylesForShapes : List (String, String)
stylesForShapes =
  [ ("height", "1000px")
  , ("width", "1000px")
  , ("position", "relative")
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
      nodeType = if m.editDisease then 1 else if m.editSymptoms then 2 else 4
  in div [ onClick address (Create nodeType loc), Html.Attributes.style stylesForShapes] [section [] [(viewListNodes address m), (viewListLinks m)]]

viewListNodes : Address Action -> Model -> Html
viewListNodes address m =
  div [] (List.map (viewNode address) m.nodes)

viewNode : Address Action -> (ID, N.Model) -> Html
viewNode address (id, model) =
  let context =
          N.Context
            (S.forwardTo address (Update id))
            (S.forwardTo address (always (Remove id)))
  in N.view context model

viewListLinks : Model -> Html
viewListLinks m =
  div [ style styleForLine ] [fromElement (layers (List.map (viewLinks m) m.links))]

viewLinks : Model -> (ID, ID) -> Element
viewLinks m (id1, id2) =
  let [(_ , node1)] = List.filter (\(diseaseID, _) -> diseaseID == id1) m.nodes
      [(_, node2)] = List.filter (\(diseaseID, _) -> diseaseID == id2) m.nodes
  in
    collage 1000 1000 [traced { defaultLine | width <- 7} (segment (linePosition node1.location) (linePosition node2.location))]

buttonStyle : Bool -> Bool -> List (String, String)
buttonStyle bool1 bool2 =
  let color =
    if bool1 then "#9c27b0" else if bool2 then "#D32F2F" else "#2196F3"
  in [ ("background-color", color), ("display", "inline"), ("color", "#fff"), ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)"), ("height", "30px"), ("margin", "0 auto")]

linePosition : (Int, Int) -> (Float, Float)
linePosition (x, y) =
  (Basics.toFloat x - 500, 550 - Basics.toFloat y)


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

