module SearchUI
  ( Model
  , Action(..)
  , view
  ) where


import Signal exposing (Address, forwardTo)
import Html exposing (Html, div, select, option, text)
import Html.Attributes exposing (id, class, value, selected)
import SearchUI.Field as Field
import SearchUI.SearchItem as SearchItem


type alias Fields = List Field.Field


type alias SearchItems = List SearchItem.Model                  


type alias Model =
  { fields : Fields
  , items  : SearchItems
  }                 


type Action = NoOp
            | SearchItemAction SearchItem.Action

viewEmptyOption : Html
viewEmptyOption =
  option [] []

         
viewFieldOption : Address Action -> SearchItem.Model -> Field.Field -> Html
viewFieldOption address item field =
  option
    [ value field.id
    , selected <| item.id == field.id
    ]
    [ text field.label ]

  
viewFieldSelect : Address Action -> Fields -> SearchItem.Model -> Html
viewFieldSelect address fields item =
  div
    [ class "col-md-4" ]
    [ select
        []
        (viewEmptyOption :: (List.map (viewFieldOption address item) fields))
    ]


viewItem : Address Action -> Fields -> SearchItem.Model -> Html
viewItem address fields item =
  div
    [ class "form-group" ]
    ( (viewFieldSelect address fields item)
      :: (SearchItem.view (forwardTo address SearchItemAction) item)
    )

      
view : Address Action -> Model -> Html                 
view address model =
  div [class "container"]
      ( (List.map (viewItem address model.fields) model.items)
        ++ [viewItem address model.fields SearchItem.empty]
      )
