module SearchUI.SearchItem
  ( Model
  , Action(..)
  , empty
  , view
  ) where


import Signal exposing (Address)
import Html exposing (Html, input, div, label, text, textarea, select, option, span)
import Html.Attributes exposing (class)
import SearchUI.Field as Field


type alias Model =
  { id  : String
  }


type Action = NoOp


type Operator = Empty
              | Equal
              | Between


empty : Model
empty =
  { id = ""
  }


view : Address Action -> Model -> List Html  
view address model =
  [ div [class "col-md-4"] [] ]
