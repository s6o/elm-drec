module BaseRecord exposing
    ( BaseFields(..)
    , BaseRecord
    , fieldLabel
    , fieldNames
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
    DRec.init
        |> field Checky DBool
        |> fieldWithMessage Booly DBool "Accepted values are: True, False"
        |> field Chary DChar
        |> field Floaty DFloat
        |> field Inty DInt
        |> field Posixy DPosix
        |> field Stringy DString


fieldNames : BaseRecord -> List BaseFields
fieldNames r =
    DRec.fieldNames r


fieldLabel : BaseFields -> String
fieldLabel bf =
    (Debug.toString >> String.dropRight 1) bf
