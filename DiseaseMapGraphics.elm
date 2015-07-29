module DiseaseMapGraphics where

import DiseaseGraphics as D
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Input exposing (..)
import Signal as S exposing (..)

type alias ID = Int

type alias Model =
  { diseases : List(ID, D.Model)
  , canEdit : Bool
  , name : String
  , uid : Int
  , locations : List(ID, (Int, Int))
  }

initialModel = 
  { diseases = []
  , canEdit = False
  , name = ""
  , uid = 0
  , locations = []
  }

type Action
  = NoOp
  | UpdateTitle String
  | UpdateDisease ID D.Action
  | CreateDisease (Int, Int)
  | Edit
  | Remove ID

actions : Mailbox Action
actions = S.mailbox NoOp

update : Action -> Model -> Model
update action m =
  case action of
    NoOp -> m

    UpdateTitle str ->
      { m | name <- str }

    CreateDisease loc ->
      if m.canEdit then
        { m | 
          uid <- m.uid + 1,
          name <- "",
          diseases <- (m.uid, (D.init m.uid loc)) :: m.diseases,
          locations <- (m.uid, loc) :: m.locations,
          canEdit <- not m.canEdit
        }
      else m
      

    UpdateDisease id diseaseAction ->
      let updateDisease (diseaseID, diseaseModel) =
        if diseaseID == id then (diseaseID, D.update diseaseAction diseaseModel) else (diseaseID, diseaseModel)
      in
        { m | diseases <- List.map updateDisease m.diseases }

    Edit ->
      { m | canEdit <- not m.canEdit }

type alias Input =
  { point : (Int, Int) }

view : Address Action -> Model -> Input -> Element
view address m input =
  let loc = input.point
  in
    layers 
      [ collage 4000 2000 [ List.map (toForm viewDisease) m.diseases ] ]
        |> clickable (CreateDisease loc)

viewDisease : Address Action -> (ID, D.Model) -> Element
viewDisease =
  let context =
    D.Context (S.forwardTo )