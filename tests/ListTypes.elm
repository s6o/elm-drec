module ListTypes exposing (Fields(..), fieldNames, init, json, values)

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
        |> DRec.field Booly (DList VBool)
        |> DRec.field Chary (DList VChar)
        |> DRec.field Floaty (DList VFloat)
        |> DRec.field Inty (DList VInt)
        |> DRec.field Jsony (DList VJson)
        |> DRec.field Stringy (DList VString)


values : DRec Fields
values =
    init
        |> DRec.setList Booly DRec.fromBool [ True, False, True ]
        |> DRec.setList Chary DRec.fromCharCode [ 65, 66, 67 ]
        |> DRec.setList Floaty DRec.fromFloat [ 1.1, 1.2, 1.3 ]
        |> DRec.setList Inty DRec.fromInt [ 1, 3, 5, 7 ]
        |> DRec.setList Jsony DRec.fromJson [ Json.Encode.string "J", Json.Encode.string "S", Json.Encode.string "O", Json.Encode.string "N" ]
        |> DRec.setList Stringy DRec.fromString [ "lorem", "ipsum" ]


json : String
json =
    """{"booly":[true,false,true],"chary":[65,66,67],"floaty":[1.1,1.2,1.3],"inty":[1,3,5,7],"jsony":["J","S","O","N"],"stringy":["lorem","ipsum"]}"""
