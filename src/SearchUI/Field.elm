module SearchUI.Field
  ( FieldType(..)
  , Field
  , init
  ) where


type FieldType = Number
               | Text
               | Options
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
