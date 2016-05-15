module SearchUI.Field exposing
  ( FieldType(..)
  , Field
  , init
  , empty
  )


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


init : String -> FieldType -> String -> Field
init id type' label =
  { id    = id
  , type' = type'
  , label = label
  }


empty : Field
empty =
  init "" None ""
