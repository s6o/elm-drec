module BaseRecord exposing
    ( BaseFields(..)
    , BaseRecord
    , fieldNames
    , schema
    )

import DRec exposing (DRec, DType(..), field, fieldWithMessage)


type BaseFields
    = Booly
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
        |> fieldWithMessage Booly DBool "Only accepted values are: true, false"
        |> field Chary DChar
        |> field Floaty DFloat
        |> field Inty DInt
        |> field Posixy DPosix
        |> field Stringy DString


fieldNames : BaseRecord -> List String
fieldNames r =
    DRec.fieldNames r
        |> List.map (Debug.toString >> String.dropRight 1 >> (++) "D")
