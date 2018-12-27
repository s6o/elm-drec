module ArrayTypes exposing (ArrayFields(..), arraydrec, arrayjson, arrayvalues, fieldNames)

import DRec exposing (DRec, DType(..), DValue(..))
import Json.Encode


type ArrayFields
    = Booly
    | Chary
    | Floaty
    | Inty
    | Jsony
    | Stringy


fieldNames : List ArrayFields
fieldNames =
    [ Booly
    , Chary
    , Floaty
    , Inty
    , Jsony
    , Stringy
    ]


arraydrec : DRec ArrayFields
arraydrec =
    DRec.init
        |> DRec.field Booly (DArray VBool)
        |> DRec.field Chary (DArray VChar)
        |> DRec.field Floaty (DArray VFloat)
        |> DRec.field Inty (DArray VInt)
        |> DRec.field Jsony (DArray VJson)
        |> DRec.field Stringy (DArray VString)


arrayvalues : DRec ArrayFields
arrayvalues =
    arraydrec



{-
   |> DRec.setBool Booly True
   |> DRec.setChar Chary 'C'
   |> DRec.setFloat Floaty 3.14
   |> DRec.setInt Inty 1357
   |> DRec.setJson Jsony (Json.Encode.string "JSON")
   |> DRec.setString Stringy "lorem ipsum"
-}


arrayjson : String
arrayjson =
    """{"booly":[true,false,true],"chary":[65,66,67],"floaty":[1.1,1.2,1.3],"inty":[1,3,5,7],"jsony":["J","S","O","N"],"stringy":["lorem","ipsum"]}"""
