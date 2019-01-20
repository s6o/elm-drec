module BaseRecord exposing
    ( BaseFields(..)
    , BaseRecord
    , fieldLabel
    , fieldNames
    , schema
    , update
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
        |> fieldWithMessage Booly DBool "Only accepted values are: true, false"
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


update : BaseFields -> String -> BaseRecord -> BaseRecord
update field str drec =
    let
        validators =
            [ ( Booly, boolValidator )
            , ( Chary, String.toInt >> Maybe.map (Char.fromCode >> DRec.fromChar) )
            , ( Floaty, String.toFloat >> Maybe.map DRec.fromFloat )
            , ( Inty, String.toInt >> Maybe.map DRec.fromInt )
            , ( Posixy, String.toInt >> Maybe.map DRec.fromPosixEpoch )
            , ( Stringy, DRec.fromString >> Just )
            ]
                |> List.map (\( f, v ) -> ( Debug.toString f, v ))
                |> Dict.fromList
    in
    validators
        |> Dict.get (Debug.toString field)
        |> Maybe.map (\v -> DRec.setWith field v str drec)
        |> Maybe.withDefault drec
