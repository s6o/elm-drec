module ArrayTypes exposing (Fields(..), fieldNames, init, json, values)

import Array
import DRec exposing (DRec, DType(..), DValue(..))
import Json.Encode


type Fields
    = Booly
    | Chary
    | Floaty
    | Inty
    | Jsony
    | Stringy


fieldNames : List Fields
fieldNames =
    [ Booly
    , Chary
    , Floaty
    , Inty
    , Jsony
    , Stringy
    ]


init : DRec Fields
init =
    DRec.init
        |> DRec.field Booly (DArray VBool)
        |> DRec.field Chary (DArray VChar)
        |> DRec.field Floaty (DArray VFloat)
        |> DRec.field Inty (DArray VInt)
        |> DRec.field Jsony (DArray VJson)
        |> DRec.field Stringy (DArray VString)


values : DRec Fields
values =
    init
        |> DRec.setArray Booly DRec.fromBool (Array.fromList [ True, False, True ])
        |> DRec.setArray Chary DRec.fromCharCode (Array.fromList [ 65, 66, 67 ])
        |> DRec.setArray Floaty DRec.fromFloat (Array.fromList [ 1.1, 1.2, 1.3 ])
        |> DRec.setArray Inty DRec.fromInt (Array.fromList [ 1, 3, 5, 7 ])
        |> DRec.setArray Jsony DRec.fromJson (Array.fromList [ Json.Encode.string "J", Json.Encode.string "S", Json.Encode.string "O", Json.Encode.string "N" ])
        |> DRec.setArray Stringy DRec.fromString (Array.fromList [ "lorem", "ipsum" ])


json : String
json =
    """{"booly":[true,false,true],"chary":[65,66,67],"floaty":[1.1,1.2,1.3],"inty":[1,3,5,7],"jsony":["J","S","O","N"],"stringy":["lorem","ipsum"]}"""
