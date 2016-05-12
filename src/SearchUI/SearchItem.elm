module SearchUI.SearchItem
  ( Operator(..)
  , Model
  , Action(..)
  , Value(..)
  , empty
  , update
  , view
  ) where


import Signal exposing (Address)
import Html exposing (Html, Attribute, input, div, label, text, select, option)
import Html.Attributes exposing (class, value, selected)
import Html.Events exposing (on, targetValue)
import SearchUI.Field as Field


type Operator = Empty
              | Equal
--              | Between
                

type alias Model =
  { field    : Field.Field
  , operator : Operator
  , value    : Maybe Value
  }


type Value = ScalarValue String
--           | ListValue (List String)


type Action = UpdateField Field.Field
            | UpdateOperator Operator
            | UpdateValue String


empty : Model
empty =
  { field    = Field.empty
  , operator = Equal
  , value    = Nothing
  }


update : Action -> Model -> Model
update action model =
  case action of
    UpdateField newField ->
      -- when a new field is set, reset everything
      { model | field = newField, operator = Equal, value = Nothing }
    UpdateOperator maybeNewOperator ->
      let
        value = Nothing -- todo: perhaps values per operator type
      in
        { model | operator = maybeNewOperator, value = value }
    UpdateValue newValue ->
      { model | value = Just <| ScalarValue newValue}


operatorsFor : Field.FieldType -> List Operator
operatorsFor fieldType =
  case fieldType of
    Field.None ->
      []
    Field.Number ->
      [Empty, Equal{-, Between-}]
    Field.Text ->
      [Empty, Equal]
    Field.Options _ ->
      [Empty, Equal]
    Field.Date ->
      [Empty, Equal{-, Between-}]


operatorFromString : String -> Operator
operatorFromString operatorString =
  case operatorString of
    "Empty" ->
      Empty
    "Equal" ->
      Equal
    --"Between" ->
    --  Between
    _ ->
      Debug.crash <| "Unknown operator: " ++ operatorString


scalarValueToString maybeValue =
  case maybeValue of
    Nothing ->
      ""
    Just (ScalarValue value) ->
      value


{-| Takes the on input handler attribute and the value
-}
viewInput : Attribute -> String -> String -> Html
viewInput onInput stringValue extraClasses =
   input
        [ class <| "form-control mb15 " ++ extraClasses
        , onInput
        , value stringValue
        ]
        [ ]


viewEqualInput : Address Action -> Model -> List Html
viewEqualInput address model =
  [ div
      [ class "col-md-3" ]
      [ viewInput
          (on "input" targetValue (\value -> Signal.message address (UpdateValue value) ))
          (scalarValueToString model.value)
          ""
      ]
  ]


{-
viewBetweenInput : Address Action -> Model -> List Html
viewBetweenInput address model =
   [ div
       [ class "col-md-2" ]
       [ viewInput address model ]
   , div
       [ class "col-md-2" ]
       [ viewInput address model ]
   ]
-}


viewValues : Address Action -> Model -> List Html
viewValues address model =
  case model.operator of
    Empty ->
      [ text "" ]
    Equal ->
     viewEqualInput address model
    --Between ->
    --  viewBetweenInput address model


viewOperatorsOption : Model -> Operator -> Html
viewOperatorsOption model operator =
  option
    [ value <| toString operator
    , selected <| model.operator == operator
    ]
    [ text <| toString operator ]


     
viewOperatorsSelect : Address Action -> Model -> Html
viewOperatorsSelect address model =
  case model.field.type' of
    Field.None ->
      text ""
    _ ->
      select
        [ class "form-control input-sm mb15"
        , on "change" targetValue (\operatorString -> Signal.message address (UpdateOperator <| operatorFromString operatorString) )
        ]
        ( List.map (viewOperatorsOption model) <| operatorsFor model.field.type' )


{-| If field type is None return blank text, otherwise we render both operator select and values
-}
view : Address Action -> Model -> List Html  
view address model =
  case model.field.type' of
    Field.None ->
      [ text "" ]
    _ ->
      ( div
          [ class "col-md-2" ]
          [ viewOperatorsSelect address model ]
        :: (viewValues address model)
      )
      
