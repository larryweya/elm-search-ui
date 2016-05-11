module SearchUI.SearchItem
  ( Model
  , Action(..)
  , empty
  , update
  , view
  ) where


import Signal exposing (Address)
import Html exposing (Html, input, div, label, text, select, option)
import Html.Attributes exposing (class)
import SearchUI.Field as Field


type alias Model =
  { field : Field.Field
  }


type Action = NoOp


type Operator = Empty
              | Equal
              | Between


empty : Model
empty =
  { field = Field.init "" Field.None ""
  }


operators : List Operator
operators =
  [Empty, Equal, Between]


update : Action -> Model -> Model
update action model =
  model


viewOperatorsSelect : Address Action -> Model -> Html
viewOperatorsSelect address model =
  select [] []


view : Address Action -> Model -> List Html  
view address model =
  [ div
      [ class "col-md-4" ]
      []
  , div
      [ class "col-md-4" ]
      []
  ]
