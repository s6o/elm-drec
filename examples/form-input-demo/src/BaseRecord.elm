module BaseRecord exposing
    ( BaseFields(..)
    , BaseRecord
    , fieldLabel
    , fieldNames
    , initialValues
    , schema
    )

import DRec exposing (DField, DRec, DType(..), field, fieldWithMessage)
import Dict


type BaseFields
    = Checky
    | Booly
    | Chary
    | Floaty
    | Inty
    | Posixy
    | Stringy


type alias BaseRecord =
    DRec BaseFields


schema : BaseRecord
schema =
    DRec.initWithIndent 4
        |> field Checky DBool
        |> fieldWithMessage Booly DBool "Accepted values are: True, False"
        |> field Chary DChar
        |> field Floaty DFloat
        |> field Inty DInt
        |> field Posixy DPosix
        |> field Stringy DString


initialValues : BaseRecord
initialValues =
    schema
        |> DRec.setBool Checky True
        |> DRec.setBool Booly False
        |> DRec.setChar Chary '🙈'
        |> DRec.setFloat Floaty 3.14
        |> DRec.setInt Inty 42
        |> DRec.setPosixEpoch Posixy 1548663014000
        |> DRec.setString Stringy "lorem ipsum"


fieldNames : BaseRecord -> List BaseFields
fieldNames r =
    DRec.fieldNames r


fieldLabel : BaseFields -> String
fieldLabel bf =
    (Debug.toString >> String.dropRight 1) bf
