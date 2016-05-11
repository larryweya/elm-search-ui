module SearchUI.SearchItem
  ( Operator(..)
  , Model
  , Action(..)
  , empty
  , update
  , view
  ) where


import Signal exposing (Address)
import Html exposing (Html, input, div, label, text, select, option)
import Html.Attributes exposing (class, value, selected)
import Html.Events exposing (on, targetValue)
import SearchUI.Field as Field


type Operator = Empty
              | Equal
              | Between
                

type alias Model =
  { field    : Field.Field
  , operator : Maybe Operator
  }


type Action = UpdateField Field.Field
            | UpdateOperator (Maybe Operator)


empty : Model
empty =
  { field    = Field.empty
  , operator = Nothing
  }


update : Action -> Model -> Model
update action model =
  case action of
    UpdateField newField ->
      { model | field = Debug.log "new" newField }
    UpdateOperator maybeNewOperator ->
      model -- todo: implement


operatorsFor : Field.FieldType -> List Operator
operatorsFor fieldType =
  case fieldType of
    Field.None ->
      []
    Field.Number ->
      [Empty, Equal, Between]
    Field.Text ->
      [Empty, Equal]
    Field.Options ->
      [Empty, Equal]
    Field.Date ->
      [Empty, Equal, Between]


viewOperatorsOption : Model -> Operator -> Html
viewOperatorsOption model operator =
  option
    [ value <| toString operator
    , selected <| (Maybe.withDefault Empty model.operator) == operator
    ]
    [ text <| toString operator ]


operatorFromString : String -> Maybe Operator
operatorFromString operatorString =
  case operatorString of
    "Empty" ->
      Just Empty
    "Equal" ->
      Just Equal
    "Between" ->
      Just Between
    _ ->
      Debug.crash <| "Unknown operator: " ++ operatorString


viewOperatorsSelect : Address Action -> Model -> Html
viewOperatorsSelect address model =
  case model.field.type' of
    Field.None ->
      text ""
    _ ->
      select
        [ on "change" targetValue (\operatorString -> Signal.message address (UpdateOperator <| operatorFromString operatorString) ) ]
        ( List.map (viewOperatorsOption model) <| operatorsFor model.field.type' )


view : Address Action -> Model -> List Html  
view address model =
  [ div
      [ class "col-md-4" ]
      [ viewOperatorsSelect address model ]
  , div
      [ class "col-md-4" ]
      []
  ]
