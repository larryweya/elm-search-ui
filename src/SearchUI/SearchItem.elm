module SearchUI.SearchItem exposing
  ( Operator(..)
  , Model
  , Msg(..)
  , Value(..)
  , empty
  , update
  , view
  )


import Json.Decode
import Html exposing (Html, Attribute, input, div, label, text, select, option)
import Html.Attributes exposing (name, class, value, selected, type')
import Html.Events exposing (on, targetValue, onInput)
import SearchUI.Field as Field


type Operator
  = Empty
  | Equal
--| Between


type alias Model =
  { field    : Field.Field
  , operator : Operator
  , value    : Maybe Value
  }


type Value
  = ScalarValue String
--| ListValue (List String)


type Msg
  = UpdateField Field.Field
  | UpdateOperator Operator
  | UpdateValue String
    
    
empty : Model
empty =
  { field    = Field.empty
  , operator = Equal
  , value    = Nothing
  }


update : Msg -> Model -> Model
update msg model =
  case msg of
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
viewInput : String -> List (Attribute Msg) -> Html Msg
viewInput extraClasses extraAttributes =
   input
        ( (class <| "form-control mb15 " ++ extraClasses)
--          :: (name <| "searchui[" ++ kind ++ "][]" ++ fieldName)
          :: extraAttributes
        )
        []


{-| Hidden inputs are used when field is none or the operator does not use inputs
e.g. the Empty operator. They ensure our form output arrays are always the same length
-}
viewHiddenInput : Model -> String -> Html Msg
viewHiddenInput model kind =
  viewInput
    ""
      [ name <| "searchui[" ++ kind ++ "]" ++ "[]" ++ model.field.id
      , type' "hidden"
      ]


viewEmptyInput : Model -> List (Html Msg)
viewEmptyInput model =
  [ viewHiddenInput model "values" ]
      
      
viewEqualInput : Model -> List (Html Msg)
viewEqualInput model =
  [ div
      [ class "col-md-3" ]
      [ viewInput
          ""
          [ name <| "searchui[values][]" ++ model.field.id
          , value (scalarValueToString model.value)
          , onInput (\value -> UpdateValue value)
          ]
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


{-| We implement a view function for each operator to alow the function to decide how its
values are rendered
-}
viewValues : Model -> List (Html Msg)
viewValues model =
  case model.operator of
    Empty ->
      viewEmptyInput model
    Equal ->
     viewEqualInput model
    --Between ->
    --  viewBetweenInput address model


viewOperatorsOption : Model -> Operator -> Html Msg
viewOperatorsOption model operator =
  option
    [ value <| toString operator
    , selected <| model.operator == operator
    ]
    [ text <| toString operator ]


     
viewOperatorsSelect : Model -> Html Msg
viewOperatorsSelect model =
  select
    [ name <| "searchui[operators][]" ++ model.field.id
    , class "form-control input-sm mb15"
    , on "change" (Json.Decode.map (\operatorString -> UpdateOperator <| operatorFromString operatorString) targetValue)
    ]
    ( List.map (viewOperatorsOption model) <| operatorsFor model.field.type' )
      
      
{-| If field type is None return blank text, otherwise we render both operator select and values
-}
view : Model -> List (Html Msg)
view model =
  case model.field.type' of
    Field.None ->
      [ viewHiddenInput model "operators"
      , viewHiddenInput model "values"
      ]
    _ ->
      ( div
          [ class "col-md-2" ]
          [ viewOperatorsSelect model ]
        :: (viewValues model)
      )
