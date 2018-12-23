module Tests exposing (suite)

import BaseTypes
import DRec
import Expect exposing (Expectation)
import Json.Encode
import Test exposing (..)


suite : Test
suite =
    describe "DRec Tests"
        [ describe "Schema"
            [ test "Check expected results of DRec.init" <|
                \_ ->
                    Expect.all
                        [ DRec.hasSchema >> Expect.equal False
                        , DRec.isEmpty >> Expect.equal True
                        , DRec.isValid >> Expect.equal True
                        , DRec.errorMessages >> Expect.equal Nothing
                        , DRec.fieldNames >> Expect.equal []
                        ]
                        DRec.init
            , test "Check schema initialization with base types" <|
                \_ ->
                    Expect.all
                        [ DRec.hasSchema >> Expect.equal True
                        , DRec.isEmpty >> Expect.equal True
                        , DRec.isValid >> Expect.equal False
                        , DRec.errorMessages >> Expect.equal Nothing
                        , DRec.fieldNames >> Expect.equal BaseTypes.fieldNames
                        ]
                        BaseTypes.basedrec
            ]
        , describe "JSON interop"
            [ test "Decode base types to DRec BaseFields" <|
                \_ ->
                    Expect.all
                        [ (Result.map (DRec.get BaseTypes.Booly) >> Result.andThen DRec.toBool) >> Expect.equal (Ok True)
                        , (Result.map (DRec.get BaseTypes.Chary) >> Result.andThen DRec.toChar) >> Expect.equal (Ok 'C')
                        , (Result.map (DRec.get BaseTypes.Floaty) >> Result.andThen DRec.toFloat) >> Expect.equal (Ok 3.14)
                        , (Result.map (DRec.get BaseTypes.Inty) >> Result.andThen DRec.toInt) >> Expect.equal (Ok 1357)
                        , (Result.map (DRec.get BaseTypes.Jsony) >> Result.andThen DRec.toJson) >> Expect.equal (Ok <| Json.Encode.string "JSON")
                        , (Result.map (DRec.get BaseTypes.Stringy) >> Result.andThen DRec.toString) >> Expect.equal (Ok "lorem ipsum")
                        ]
                        (DRec.decodeString BaseTypes.basedrec BaseTypes.basejson)
            , test "Encode DRec BaseFields into JSON" <|
                \_ ->
                    let
                        json =
                            BaseTypes.basevalues
                                |> DRec.encoder
                                |> Json.Encode.encode 0
                    in
                    Expect.equal BaseTypes.basejson json
            ]
        ]
