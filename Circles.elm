module Circles where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Signal as S exposing (..)
import Mouse as M
import Time exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import ElmFire exposing (..)
import Json.Encode as JE exposing (..)
import Json.Decode as JD exposing ((:=))
import Task exposing (Task, andThen)
import Dict as D exposing (..)

{--Type declarations -}

type alias ID = String

type alias Input = {
    point : (Int, Int)
  }

type NodeType
    = Disease
    | Symptom
    | Empty

type alias PreNode =
  { title : String
  , location1 : Int
  , location2 : Int
  , nodeType : String
  }

nodeThePreNode : PreNode -> Node
nodeThePreNode preNode =
  let title = preNode.title
      loc = (preNode.location1, preNode.location2)
      typeOfNode = if (preNode.nodeType == "Disease") then Disease else if (preNode.nodeType == "Symptom") then Symptom else Empty
  in  (initNode title loc typeOfNode False)

type alias Node =
  { title : String
  , location : (Int, Int)
  , nodeType : NodeType
  , autoFocus : Bool
  }

emptyNode : Node
emptyNode =
  initNode "" (0,0) Empty False

type alias Model =
  { nodes : Dict ID Node
  , editDisease : Bool
  , editSymptoms : Bool
  , links : List (ID, ID)
  , storedDisease : Int
  }

{--Initial Model -}

emptyModel = 
  { nodes = D.empty
  , editDisease = False
  , editSymptoms = False
  , links = []
  , storedDisease = 0
  }

{--Initializing Nodes-}

initNode : String -> (Int, Int) -> NodeType -> Bool -> Node
initNode str loc nodeType bool =
  { title = str
  , location = loc
  , nodeType = nodeType
  , autoFocus = bool
  }

{--Deal with user input-}

type GuiEvent
  = NoGuiEvent
  | UpdateNode ID NodeAction
  | AddNode NodeType (Int, Int)
  | Edit
  | DeleteNode ID

guiInput : Mailbox GuiEvent
guiInput = S.mailbox NoGuiEvent

type alias GuiAddress = Address GuiEvent

{--Deal with server input-}

type ServerEvent
  = NoServerEvent
  | Added (ID, PreNode)
  | Changed (ID, PreNode)
  | Removed ID

serverInput : Mailbox ServerEvent
serverInput = S.mailbox NoServerEvent

{--Manage tasks-}

type Effects
  = NoEffect
  | SingleTask (Task Never ())
  | Sequential (List Effects)
  | Concurrent (List Effects)

type Never = Never Never

effect : Task x a -> Effects
effect task =
  SingleTask <| Task.map (always ()) (Task.toResult task)

effectAsync : Task x a -> Effects
effectAsync task =
  SingleTask <| Task.map (always ()) (Task.spawn task)

effectsToTask : Effects -> Task Never ()
effectsToTask effects =
  case effects of

    NoEffect -> Task.succeed ()

    SingleTask task ->
      task

    Sequential listOfEffects ->
      List.map effectsToTask listOfEffects
        |> Task.sequence
        |> Task.map (always ())

    Concurrent listOfEffects ->
      List.map (effectsToTask >> Task.spawn) listOfEffects
        |> Task.sequence
        |> Task.map (always ())

{--Combine GUI and Server Interactions -}

type Action
  = FromGui GuiEvent
  | FromServer ServerEvent

actions : Signal Action
actions =
  S.merge
    (S.map FromGui guiInput.signal)
    (S.map FromServer serverInput.signal)

state : Signal (Model, Effects)
state =
  S.foldp
    updateState
    (emptyModel, NoEffect)
    actions

port runEffects : Signal (Task Never ())
port runEffects =
  S.map effectsToTask effects

{--Update the state -}

updateState : Action -> (Model, Effects) -> (Model, Effects)
updateState action (model, _) =
  case action of
    _ -> (model, NoEffect)

    FromServer (Added (id, preNode)) ->
      let newNode = (nodeThePreNode preNode)
      in  
        ( { model | nodes <- D.insert id newNode model.nodes }
        , NoEffect
        )

    FromServer (Changed (id, preNode)) ->
      let modifiedNode = (nodeThePreNode preNode)
      in  ( { model | nodes <- D.insert id modifiedNode model.nodes }
        , NoEffect
        )

    FromServer (Removed id) ->
      ( { model | nodes <- D.remove id model.nodes }
      , NoEffect
      )

    FromGui (AddNode nodeType loc) ->
      let newNode = ((initNode "" loc nodeType True))
      in  ( model 
          , effectAsync <|
              set
                ( encoderItem newNode )
                ( fromUrl url |> push )
          )

   {-- FromGui (UpdateNode id nodeAction) ->
      let updateNode (nodeId, nodeModel) =
        if nodeId == id then
          ( model
          , effectAsync <|
              ElmFire.update
                ( encoderItem (nodeUpdate nodeAction nodeModel) )
                ( fromUrl url |> ElmFire.sub id )
          )
        else (model, NoEffect)
      in
        List.map updateNode (D.toList model.nodes) -}

    FromGui (DeleteNode id) ->
      ( model 
      , effectAsync <|
          ElmFire.remove
            (fromUrl url |> ElmFire.sub id)
      )

    FromGui (Edit) ->
      ( { model | editDisease <- not model.editDisease }
      , NoEffect
      )

type NodeAction
    = UpdateTitle String
    | ChangeBool


nodeUpdate : NodeAction -> Node -> Node
nodeUpdate nodeAction node =
  case nodeAction of
    UpdateTitle str ->
        {node | title <- str}

    ChangeBool ->
        {node | autoFocus <- True}
      

{-- Styles, Views and Positioning -}

drawStyle : List (String, String)
drawStyle =
  [ ("height", "1000px")
  , ("width", "1000px")
  , ("position", "relative")
  , ("z-index", "1")
  ]

view : GuiAddress -> Model -> Input -> Html
view guiAddress m input =
  body [class "disease-map-wrapper", Html.Attributes.style [("position", "absolute")]]
  [ section
    [ id "disease-map-app" ]
    [ (lazy3 buttonBar guiAddress m input)
    , (lazy3 combinedSpace guiAddress m input)
    ] 
  ]   

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

buttonBar : GuiAddress -> Model -> Input -> Html
buttonBar guiAddress m input =
  let editButton = Html.button [ (Html.Attributes.style (buttonStyle m.editDisease m.editSymptoms)), onClick guiAddress Edit ] [ Html.text "Create New Disease" ]
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

combinedSpace : GuiAddress -> Model -> Input -> Html
combinedSpace guiAddress m input =
  let loc = input.point
      nodeType = if m.editDisease then Disease else if m.editSymptoms then Symptom else Empty
  in div [ onClick guiAddress (AddNode nodeType loc), Html.Attributes.style stylesForShapes] [section [] [(viewListNodes guiAddress m), (viewListLinks m)]]

viewListNodes : GuiAddress -> Model -> Html
viewListNodes guiAddress m =
  let nodeView (id, nodeModel) = div [] [(viewNode guiAddress (id, nodeModel))]
  in  div [] (List.map nodeView (D.toList m.nodes))

viewNode : GuiAddress -> (ID, Node) -> Html
viewNode guiAddress (id, node) =
  let 
      html place color node =
        div [ style (layout color node.location), onClick guiAddress (UpdateNode id ChangeBool)  ]
          [ input
              [ placeholder place
              , autofocus node.autoFocus
              , Html.Attributes.value node.title
              , on "input" targetValue (\str -> message guiAddress (UpdateNode id (UpdateTitle str)))
              , style inputLayout
              ]
          []
          , button [onClick guiAddress (DeleteNode id), style removeButton] [Html.text "X"]
          ]
  in
    case node.nodeType of
      Disease -> html "Disease Name..." "#9c27b0" node 
      
      Symptom -> html "Symptom Name..." "#D32F2F" node

viewListLinks : Model -> Html
viewListLinks m =
  div [ style styleForLine ] [fromElement (layers (List.map (viewLinks m) m.links))]

viewLinks : Model -> (ID, ID) -> Element
viewLinks m (id1, id2) =
  let node1 = Maybe.withDefault emptyNode (D.get id1 m.nodes)
      node2 = Maybe.withDefault emptyNode (D.get id2 m.nodes)
  in
    collage 1000 1000 [traced { defaultLine | width <- 7 } (segment (linePosition node1.location) (linePosition node2.location))]

buttonStyle : Bool -> Bool -> List (String, String)
buttonStyle bool1 bool2 =
  let color =
    if bool1 then "#9c27b0" else if bool2 then "#D32F2F" else "#2196F3"
  in [ ("background-color", color), ("display", "inline"), ("color", "#fff"), ("box-shadow", "0 5px 11px 0 rgba(0, 0, 0, 0.18), 0 4px 15px 0 rgba(0, 0, 0, 0.15)"), ("height", "30px"), ("margin", "0 auto")]

linePosition : (Int, Int) -> (Float, Float)
linePosition (x, y) =
  (Basics.toFloat x - 500, 550 - Basics.toFloat y)

initialModel : Model
initialModel = emptyModel


{-- Deal with all the Signals -}

main : Signal Html
main = view guiInput.address <~ flowModel ~ userInput

delta : Signal Time
delta = S.map (\t -> t / 1500) (fps 10)

userInput : Signal Input
userInput = 
  S.sampleOn delta <|
    S.map Input M.position

flowModel : Signal Model
flowModel = S.map fst state

effects : Signal Effects
effects = S.map snd state

{-- Interact with Firebase-}

url : String
url = "https://disease-map.firebaseIO.com"

encoderItem : Node -> JE.Value
encoderItem singleNode =
  let nType = toString singleNode.nodeType
      (a, b) = singleNode.location
  in  JE.object
        [ ("title", JE.string singleNode.title )
        , ("location1", JE.int a)
        , ("location2", JE.int b)
        , ("nodeType", JE.string nType)
        ] 

port runServerQuery : Task ElmFire.Error ()
port runServerQuery =
  let snap2task : ((ID, PreNode) -> ServerEvent) -> Snapshot -> Task () ()
      snap2task eventOp =
        (\snapshot ->
          case decodeItem snapshot.value of
            Just nodeModel ->
              Signal.send
                serverInput.address
                (eventOp (snapshot.key, nodeModel))
            Nothing -> Task.fail ()
        )
      doNothing = \_ -> Task.succeed ()
      loc = (fromUrl url)
  in
    subscribe
      (snap2task Added) doNothing childAdded loc
    `andThen`
    \_ -> subscribe
      (snap2task Changed) doNothing childChanged loc
    `andThen`
    \_ -> subscribe
      (snap2task (\(id, _) -> Removed id)) doNothing childRemoved loc
    `andThen`
    \_ -> Task.succeed ()



decodeItem : JD.Value -> Maybe PreNode
decodeItem value =
  JD.decodeValue decoderItem value |> Result.toMaybe

decoderItem : JD.Decoder PreNode
decoderItem =
  ( JD.object4 PreNode
      ("title" := JD.string )
      ("location1" := JD.int)
      ("location2" := JD.int)
      ("nodeType" := JD.string)
  )




