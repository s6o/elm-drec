module BaseTypes exposing (Fields(..), fieldNames, init, json, values)

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
        |> DRec.field Booly DBool
        |> DRec.field Chary DChar
        |> DRec.field Floaty DFloat
        |> DRec.field Inty DInt
        |> DRec.field Jsony DJson
        |> DRec.field Stringy DString


values : DRec Fields
values =
    init
        |> DRec.setBool Booly True
        |> DRec.setChar Chary 'C'
        |> DRec.setFloat Floaty 3.14
        |> DRec.setInt Inty 1357
        |> DRec.setJson Jsony (Json.Encode.string "JSON")
        |> DRec.setString Stringy "lorem ipsum"


json : String
json =
    """{"booly":true,"chary":67,"floaty":3.14,"inty":1357,"jsony":"JSON","stringy":"lorem ipsum"}"""
