module SearchUI
  ( Model
  , Action(..)
  , update
  , view
  ) where


import Debug
import Signal exposing (Address, forwardTo)
import Json.Decode as JsonDecode
import Json.Encode
import Html exposing (Html, div, select, option, text, a)
import Html.Attributes exposing (id, class, value, selected, href, property)
import Html.Events exposing (on, targetValue, onClick)
import SearchUI.Field as Field
import SearchUI.SearchItem as SearchItem


type alias ID = Int
              

type alias Fields = List Field.Field


type alias SearchItems = List (ID, SearchItem.Model)


type Action = AddItem
            | DeleteItem ID
            | SearchItemAction ID SearchItem.Action
            | SelectField ID String                       


type alias Model =
  { fields : Fields
  , items  : SearchItems
  , nextId : ID
  }                 


{-| Run update on the item if its id matches the provided target item's id
-}              
updateMatchingItem : ID -> SearchItem.Action -> (ID, SearchItem.Model) -> (ID, SearchItem.Model)
updateMatchingItem targetItemId itemAction (itemId, currentItem) =
  let
    item = if targetItemId == itemId then
             SearchItem.update itemAction currentItem
           else
             currentItem
  in
    (itemId, item)


{-| Find a field by its id and return the first or Nothing
-}
findField : Fields -> String -> Maybe Field.Field
findField fields id =
  List.filter (\field -> field.id == id) fields
  |> List.head


deleteItem : SearchItems -> Int -> SearchItems  
deleteItem items itemId =
  List.filter (\(id, item) -> id /= itemId) items


update : Action -> Model -> Model
update action model =
  case action of
    AddItem ->
      
      { model |
          items = model.items ++ [ (model.nextId, SearchItem.empty) ],
          nextId = model.nextId + 1
      }
    DeleteItem itemId ->
      { model | items = deleteItem model.items itemId }
    SearchItemAction itemId itemAction ->
      { model | items = List.map (updateMatchingItem itemId itemAction) model.items }
    SelectField itemId newFieldId ->
      let
        -- if the fieldId cannot be found - ideally the empty default option, return an empty field
        newField = findField model.fields newFieldId
                   |> Maybe.withDefault Field.empty
      in
        { model |
            items = List.map (updateMatchingItem itemId <| SearchItem.UpdateField newField) model.items
        }
    

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


--targetSelectedIndex =
--  JsonDecode.at ["target", "selectedIndex"] JsonDecode.int      

  
viewFieldSelect : Address Action -> Fields -> ID -> SearchItem.Model -> Html
viewFieldSelect address fields itemId item =
  div
    [ class "col-md-3" ]
    [ select
        [ class "form-control input-sm mb15"
        , on "change" targetValue (\fieldId -> Signal.message address (SelectField itemId fieldId) )
        ]
        (viewEmptyOption :: (List.map (viewFieldOption address item) fields))
    ]


viewDeleteItem : Address Action -> ID -> Html
viewDeleteItem address itemId =
  div
    [ class "pull-left" ]
    [ a
        [ href "javascript:;"
        , onClick address <| DeleteItem itemId
        , property "innerHTML" (Json.Encode.string "&times;")
        ]
        []
    ]


viewItem : Address Action -> Fields -> (ID, SearchItem.Model) -> Html
viewItem address fields (itemId, item) =
  div
    [ class "form-group" ]
    ( (viewDeleteItem address itemId)
      :: (viewFieldSelect address fields itemId item)
      :: (SearchItem.view (forwardTo address <| SearchItemAction itemId) item)
    )


viewAddItem : Address Action -> Model -> Html
viewAddItem address model =
  div
    [ class "row" ]
    [ a
        [ class "btn"
        , href "javascript:;"
        , onClick address AddItem
        ]
        [ text "+ Add Filter" ]
    ]

      
view : Address Action -> Model -> Html                 
view address model =
  div [ class "container" ]
      ( (List.map (viewItem address model.fields) model.items)
        ++ [ viewAddItem address model ]
      )
