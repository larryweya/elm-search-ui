module SearchUI.Examples.Simple where


import Signal exposing (Address, forwardTo)
import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import StartApp.Simple as StartApp
import SearchUI exposing (Model)
import SearchUI.Field as Field
import SearchUI.SearchItem as SearchItem


type Action = NoOp
            | SearchUIAction SearchUI.Action


model : Model 
model =
  { fields =
      [ Field.init "id_number" Field.Number "ID Number"
      , Field.init "gender" (Field.Options [("male", "Male"), ("female", "Female")]) "Gender"
      , Field.init "date_of_birth" Field.Date "Date of Birth"
      ]
   , items =    
      [      
        ( 0
        , { field    = Field.init "id_number" Field.Number "ID Number"
          , operator = SearchItem.Equal
          , value    = Just <| SearchItem.ScalarValue "2222"
          }
        )
{--       
      , ( 1
        , { field    = Field.init "gender" Field.Number "ID Number"
          , operator = SearchItem.Equal
          , value    = Just <| SearchItem.ListValue ["male", "female"]
          }
        )
--}
      ]
    , nextId = 2
  }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    SearchUIAction uiAction ->
      SearchUI.update uiAction model


view : Address Action -> Model -> Html    
view address model =
  SearchUI.view (forwardTo address SearchUIAction) model


main : Signal Html
main =
  StartApp.start
    { model = model
    , update = update
    , view = view
    }  
