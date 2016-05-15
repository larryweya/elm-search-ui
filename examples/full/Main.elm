port module SearchUI.Examples.Full exposing (..)


import Json.Decode exposing ((:=))
import Html exposing (Html, div, button, text, input, label)
import Html.Attributes exposing (type', id, name, value)
import Html.Events exposing (onClick)
import Html.App as App


type Msg
  = Load Json.Decode.Value

type FieldType
  = None
  | Number
  | Text
  | Options (List (String, String))
  | Date
    
    
type alias Field =
  { id    : String
  , type' : FieldType
  , label : String
  }


type alias Fields = List Field


type alias Model =
  { fields : Fields
  }


fieldTypeFromString type' =
  case type' of
    "text" ->
      Text
    "number" ->
      Number
    "date" ->
      Date
    _ ->
      None


decodeFieldType =
  Json.Decode.map fieldTypeFromString Json.Decode.string


decodeField =
  Json.Decode.object3 Field
    ("id"    := Json.Decode.string)
    ("type"  := decodeFieldType)
    ("title" := Json.Decode.string)


decodeFields =
  Json.Decode.list decodeField


decodePayload json =
  Json.Decode.decodeValue
    (Json.Decode.object1
      (\fields -> fields)
      ("fields" := decodeFields)
    )
    json


init : Json.Decode.Value -> (Model, Cmd Msg)
init fieldJson =
  let
     fieldData = (decodePayload fieldJson)
     data = case fieldData of
       Ok result ->
         result
       Err reason ->
        Debug.crash reason
  in
    ({ fields = data }, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Load json ->
      let
        newFields = case (decodePayload json) of
          Ok result ->
            result
          Err reason ->
            Debug.crash reason
      in
        ({ model | fields = Debug.log "new fields" newFields }, Cmd.none)


viewField : Field -> Html Msg
viewField field =
  div
    []
    [ label [] [ text field.label ]
    , input
        [ type' "text"
        , id field.id
        ]
        []
    ]


view : Model -> Html Msg
view model =
  div
    []
    (List.map viewField model.fields)


port fields : (Json.Decode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  fields Load


main =
  App.programWithFlags
    { init = init
    , view  = view
    , update = update
    , subscriptions = subscriptions
    }
  
