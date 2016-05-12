module SearchUI
  ( Model
  , Action(..)
  , update
  , view
  ) where


import Debug
import Signal exposing (Address, forwardTo)
import Json.Decode as JsonDecode
import Html exposing (Html, div, select, option, text)
import Html.Attributes exposing (id, class, value, selected)
import Html.Events exposing (on, targetValue)
import SearchUI.Field as Field
import SearchUI.SearchItem as SearchItem


type alias Fields = List Field.Field


type alias SearchItems = List SearchItem.Model                  


type alias Model =
  { fields : Fields
  , items  : SearchItems
  }                 


type Action = NoOp
            | SearchItemAction Int SearchItem.Action
            | SelectField Int String


{-| Run update on the item if its field id matches the target field id
-}              
updateMatchingItem : Int -> SearchItem.Action -> Int -> SearchItem.Model -> SearchItem.Model
updateMatchingItem targetIndex itemAction currentIndex currentItem =
  if targetIndex == currentIndex then
    SearchItem.update itemAction currentItem
  else
    currentItem


{-| FInd a field by its id and return the first or Nothing
-}
findField : Fields -> String -> Maybe Field.Field
findField fields id =
  List.filter (\field -> field.id == id) fields
  |> List.head


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SearchItemAction index itemAction ->
      { model | items = List.indexedMap (updateMatchingItem index itemAction) model.items }
    SelectField index newFieldId ->
      let
        -- if the fieldId cannot be found - ideally the empty default option, return an empty field
        newField = findField model.fields newFieldId
                   |> Maybe.withDefault Field.empty
      in
        { model | items = List.indexedMap (updateMatchingItem index <| SearchItem.UpdateField newField) model.items }
    

viewEmptyOption : Html
viewEmptyOption =
  option [] []

         
viewFieldOption : Address Action -> SearchItem.Model -> Field.Field -> Html
viewFieldOption address item field =
  option
    [ value field.id
    , selected <| item.field.id == field.id
    ]
    [ text field.label ]


targetSelectedIndex =
  JsonDecode.at ["target", "selectedIndex"] JsonDecode.int      

  
viewFieldSelect : Address Action -> Fields -> Int -> SearchItem.Model -> Html
viewFieldSelect address fields itemIndex item =
  div
    [ class "col-md-3" ]
    [ select
        [ class "form-control input-sm mb15"
        , on "change" targetValue (\fieldId -> Signal.message address (SelectField itemIndex fieldId) )
        ]
        (viewEmptyOption :: (List.map (viewFieldOption address item) fields))
    ]


viewItem : Address Action -> Fields -> Int -> SearchItem.Model -> Html
viewItem address fields itemIndex item =
  div
    [ class "form-group" ]
    ( (viewFieldSelect address fields itemIndex item)
      :: (SearchItem.view (forwardTo address <| SearchItemAction itemIndex) item)
    )

      
view : Address Action -> Model -> Html                 
view address model =
  div [class "container"]
      ( (List.indexedMap (viewItem address model.fields) model.items)
        ++ [viewItem address model.fields -1 SearchItem.empty]
      )
