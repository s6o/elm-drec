module MaybeTypes exposing (Fields(..), fieldNames, init, json, nojson, novalues, values)

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
        |> DRec.field Booly (DMaybe VBool)
        |> DRec.field Chary (DMaybe VChar)
        |> DRec.field Floaty (DMaybe VFloat)
        |> DRec.field Inty (DMaybe VInt)
        |> DRec.field Jsony (DMaybe VJson)
        |> DRec.field Stringy (DMaybe VString)


novalues : DRec Fields
novalues =
    init
        |> DRec.setMaybe Booly DRec.fromBool Nothing
        |> DRec.setMaybe Chary DRec.fromCharCode Nothing
        |> DRec.setMaybe Floaty DRec.fromFloat Nothing
        |> DRec.setMaybe Inty DRec.fromInt Nothing
        |> DRec.setMaybe Jsony DRec.fromJson Nothing
        |> DRec.setMaybe Stringy DRec.fromString Nothing


values : DRec Fields
values =
    init
        |> DRec.setMaybe Booly DRec.fromBool (Just True)
        |> DRec.setMaybe Chary DRec.fromCharCode (Just 65)
        |> DRec.setMaybe Floaty DRec.fromFloat (Just 1.2)
        |> DRec.setMaybe Inty DRec.fromInt (Just 3)
        |> DRec.setMaybe Jsony DRec.fromJson (Just (Json.Encode.string "J"))
        |> DRec.setMaybe Stringy DRec.fromString (Just "lorem ipsum")


json : String
json =
    """{"booly":true,"chary":65,"floaty":1.2,"inty":3,"jsony":"J","stringy":"lorem ipsum"}"""


nojson : String
nojson =
    """{}"""
