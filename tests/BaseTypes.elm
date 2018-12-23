module BaseTypes exposing (BaseFields(..), basedrec, basejson, basevalues, fieldNames)

import DRec exposing (DRec, DType(..), DValue(..))
import Json.Encode


type BaseFields
    = Booly
    | Chary
    | Floaty
    | Inty
    | Jsony
    | Stringy


fieldNames : List BaseFields
fieldNames =
    [ Booly
    , Chary
    , Floaty
    , Inty
    , Jsony
    , Stringy
    ]


basedrec : DRec BaseFields
basedrec =
    DRec.init
        |> DRec.field Booly DBool
        |> DRec.field Chary DChar
        |> DRec.field Floaty DFloat
        |> DRec.field Inty DInt
        |> DRec.field Jsony DJson
        |> DRec.field Stringy DString


basevalues : DRec BaseFields
basevalues =
    basedrec
        |> DRec.setBool Booly True
        |> DRec.setChar Chary 'C'
        |> DRec.setFloat Floaty 3.14
        |> DRec.setInt Inty 1357
        |> DRec.setJson Jsony (Json.Encode.string "JSON")
        |> DRec.setString Stringy "lorem ipsum"


basejson : String
basejson =
    """{"booly":true,"chary":67,"floaty":3.14,"inty":1357,"jsony":"JSON","stringy":"lorem ipsum"}"""
