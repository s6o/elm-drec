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
    (Debug.toString >> String.dropRight 1 >> (++) "D") bf


boolValidator : String -> Maybe (DField a)
boolValidator input =
    let
        allowed =
            [ "false", "true" ]

        linput =
            String.toLower input
    in
    if List.member linput allowed then
        Just <|
            if linput == "true" then
                DRec.fromBool True

            else
                DRec.fromBool False

    else
        Nothing
