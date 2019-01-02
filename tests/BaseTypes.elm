module BaseTypes exposing
    ( Fields(..)
    , fieldNames
    , init
    , json
    , unit
    , unitAbbrValidator
    , unitError
    , unitValues
    , values
    )

import DRec exposing (DField, DRec, DType(..), DValue(..))
import Json.Encode


type Fields
    = Booly
    | Chary
    | Floaty
    | Inty
    | Jsony
    | Posixy
    | Stringy
    | Suby
    | Abbr
    | Long


fieldNames : List Fields
fieldNames =
    [ Booly
    , Chary
    , Floaty
    , Inty
    , Jsony
    , Posixy
    , Stringy
    , Suby
    ]


unit : DRec Fields
unit =
    DRec.init
        |> DRec.field Abbr DString
        |> DRec.field Long DString


init : DRec Fields
init =
    DRec.init
        |> DRec.field Booly DBool
        |> DRec.field Chary DChar
        |> DRec.field Floaty DFloat
        |> DRec.field Inty DInt
        |> DRec.field Jsony DJson
        |> DRec.field Posixy DPosix
        |> DRec.field Stringy DString
        |> DRec.field Suby (DDRec <| DRec.schema unit)


unitError : String
unitError =
    "Unit Abbr. must be single letter only."


unitValues : DRec Fields
unitValues =
    unit
        |> DRec.setString Abbr "m"
        |> DRec.validationMessage Abbr unitError
        |> DRec.setString Long "meters"


unitAbbrValidator : String -> Maybe (DField Fields)
unitAbbrValidator abbrStr =
    if String.length abbrStr == 1 then
        DRec.fromString abbrStr
            |> Just

    else
        Nothing


values : DRec Fields
values =
    init
        |> DRec.setBool Booly True
        |> DRec.setChar Chary 'C'
        |> DRec.setFloat Floaty 3.14
        |> DRec.setInt Inty 1357
        |> DRec.setJson Jsony (Json.Encode.string "JSON")
        |> DRec.setPosixEpoch Posixy 1546300800000
        |> DRec.setString Stringy "lorem ipsum"
        |> DRec.setDRec Suby unitValues


json : String
json =
    """{"booly":true,"chary":67,"floaty":3.14,"inty":1357,"jsony":"JSON","posixy":1546300800000,"stringy":"lorem ipsum","suby":{"abbr":"m","long":"meters"}}"""
