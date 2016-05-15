module SearchUI exposing
  ( Model
  , Msg(..)
  , update
  , view
  )


import Json.Encode
import Json.Decode
import Html exposing (Html, div, select, option, text, a)
import Html.Attributes exposing (id, name, class, value, selected, href, property)
import Html.Events exposing (on, targetValue, onClick)
import Html.App as App
import SearchUI.Field as Field
import SearchUI.SearchItem as SearchItem


type alias ID = Int
              

type alias Fields = List Field.Field


type alias SearchItems = List (ID, SearchItem.Model)


type Msg
  = AddItem (Maybe SearchItem.Model)
  | DeleteItem ID
  | SearchItemMsg ID SearchItem.Msg
  | SelectField ID String       
    
    
type alias Model =
  { fields : Fields
  , items  : SearchItems
  , nextId : ID
  }                 


{-| Run update on the item if its id matches the provided target item's id
-}              
updateMatchingItem : ID -> SearchItem.Msg -> (ID, SearchItem.Model) -> (ID, SearchItem.Model)
updateMatchingItem targetItemId itemMsg (itemId, currentItem) =
  let
    item = if targetItemId == itemId then
             SearchItem.update itemMsg currentItem
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


update : Msg -> Model -> Model
update msg model =
  case msg of
    AddItem maybeItem ->
      let
        item = Maybe.withDefault SearchItem.empty maybeItem
      in
        { model |
            items = model.items ++ [ (model.nextId, item) ],
            nextId = model.nextId + 1
        }
    DeleteItem itemId ->
      { model | items = deleteItem model.items itemId }
    SearchItemMsg itemId itemMsg ->
      { model | items = List.map (updateMatchingItem itemId itemMsg) model.items }
    SelectField itemId newFieldId ->
      let
        -- if the fieldId cannot be found - ideally the empty default option, return an empty field
        newField = findField model.fields newFieldId
                 |> Maybe.withDefault Field.empty
      in
        { model |
            items = List.map (updateMatchingItem itemId <| SearchItem.UpdateField newField) model.items
        }
    

viewEmptyOption : Html Msg
viewEmptyOption =
  option [] []

         
viewFieldOption : SearchItem.Model -> Field.Field -> Html Msg
viewFieldOption item field =
  option
    [ value field.id
    , selected <| item.field.id == field.id
    ]
    [ text field.label ]

  
viewFieldSelect : Fields -> ID -> SearchItem.Model -> Html Msg
viewFieldSelect fields itemId item =
  div
    [ class "col-md-3" ]
    [ select
        [ name <| "searchui[filters][]" ++ item.field.id
        , class "form-control input-sm mb15"
        , on "change" (Json.Decode.map (\fieldId -> SelectField itemId fieldId) targetValue)
        ]
        (viewEmptyOption :: (List.map (viewFieldOption item) fields))
    ]


viewDeleteItem : ID -> Html Msg
viewDeleteItem itemId =
  div
    [ class "pull-left" ]
    [ a
        [ href "javascript:;"
        , onClick <| DeleteItem itemId
        , property "innerHTML" (Json.Encode.string "&times;")
        ]
        []
    ]


viewItem : Fields -> (ID, SearchItem.Model) -> Html Msg
viewItem fields (itemId, item) =
  div
    [ class "form-group" ]
    ( (viewDeleteItem itemId)
      :: (viewFieldSelect fields itemId item)
      :: List.map
           (\itemView -> App.map (SearchItemMsg itemId) itemView)
           (SearchItem.view item)
    )


viewAddItem : Model -> Html Msg
viewAddItem model =
  div
    [ class "row" ]
    [ a
        [ class "btn"
        , href "javascript:;"
        , onClick <| AddItem Nothing
        ]
        [ text "+ Add Filter" ]
    ]

      
view : Model -> Html Msg
view model =
  div [ class "container" ]
      ( (List.map (viewItem model.fields) model.items)
        ++ [ viewAddItem model ]
      )
