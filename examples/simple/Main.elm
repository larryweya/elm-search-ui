module SearchUI.Examples.Simple exposing(..)


import Html exposing (Html, div)
import Html.Attributes exposing (id, class)
import Html.App as App
import SearchUI exposing (Model)
import SearchUI.Field as Field
import SearchUI.SearchItem as SearchItem


type Msg = SearchUIMsg SearchUI.Msg


model : Model 
model =
  { fields =
      [ Field.init "id_number" Field.Number "ID Number"
      , Field.init "gender" (Field.Options [("male", "Male"), ("female", "Female")]) "Gender"
      , Field.init "date_of_birth" Field.Date "Date of Birth"
      , Field.init "valuation" Field.Number "Annual Rent"
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


update : Msg -> Model -> Model
update msg model =
  case msg of
    SearchUIMsg uiMsg ->
      SearchUI.update uiMsg model


view : Model -> Html Msg
view model =
  App.map SearchUIMsg (SearchUI.view model)


main =
  App.beginnerProgram
    { model = model
    , update = update
    , view = view
    }  
