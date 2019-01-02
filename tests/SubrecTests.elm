module SubrecTests exposing (subrecsuite)

import DRec
import Expect exposing (Expectation)
import Json.Encode
import SubrecTypes
import Test exposing (..)


subrecsuite : Test
subrecsuite =
    describe "Nested sub-record tests"
        [ test "decoding" <|
            \_ ->
                let
                    drec =
                        DRec.decodeString SubrecTypes.init SubrecTypes.json
                            |> Result.withDefault SubrecTypes.init

                    entry =
                        DRec.get SubrecTypes.Entry
                            >> DRec.toDRec
                            >> Result.withDefault SubrecTypes.initEntry

                    unit =
                        DRec.get SubrecTypes.Unit
                            >> DRec.toDRec
                            >> Result.withDefault SubrecTypes.initUnit
                in
                Expect.all
                    [ DRec.get SubrecTypes.Name >> DRec.toString >> Expect.equal (Ok "Level 0")
                    , entry >> DRec.get SubrecTypes.Description >> DRec.toString >> Expect.equal (Ok "Tower measurement")
                    , entry >> DRec.get SubrecTypes.Measurement >> DRec.toFloat >> Expect.equal (Ok 314.5)
                    , entry >> unit >> DRec.get SubrecTypes.Abbr >> DRec.toString >> Expect.equal (Ok "m")
                    , entry >> unit >> DRec.get SubrecTypes.Long >> DRec.toString >> Expect.equal (Ok "meters")
                    ]
                    drec
        , test "encoding" <|
            \_ ->
                let
                    jsonres =
                        DRec.encoder SubrecTypes.values
                            |> Json.Encode.encode 0
                in
                Expect.equal SubrecTypes.json jsonres
        , test "pretty encoding" <|
            \_ ->
                let
                    jsonres =
                        SubrecTypes.valuesPretty
                            |> DRec.stringify
                in
                Expect.equal SubrecTypes.prettyJson jsonres
        ]
