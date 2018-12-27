module BaseTests exposing (basesuite)

import BaseTypes
import DRec
import Expect exposing (Expectation)
import Json.Encode
import Test exposing (..)


basesuite : Test
basesuite =
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
        , describe "DRec getters and setters"
            [ test "get as Bool" <|
                \_ -> Expect.equal (Ok True) (DRec.get BaseTypes.Booly BaseTypes.basevalues |> DRec.toBool)
            , test "get as Char" <|
                \_ -> Expect.equal (Ok 'C') (DRec.get BaseTypes.Chary BaseTypes.basevalues |> DRec.toChar)
            , test "get as Float" <|
                \_ -> Expect.equal (Ok 3.14) (DRec.get BaseTypes.Floaty BaseTypes.basevalues |> DRec.toFloat)
            , test "get as Int" <|
                \_ -> Expect.equal (Ok 1357) (DRec.get BaseTypes.Inty BaseTypes.basevalues |> DRec.toInt)
            , test "get as Json.Encode.Value" <|
                \_ -> Expect.equal (Ok <| Json.Encode.string "JSON") (DRec.get BaseTypes.Jsony BaseTypes.basevalues |> DRec.toJson)
            , test "get as String" <|
                \_ -> Expect.equal (Ok "lorem ipsum") (DRec.get BaseTypes.Stringy BaseTypes.basevalues |> DRec.toString)
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
        , describe "Test validations via setWith"
            [ test "failed Int assignment" <|
                \_ ->
                    let
                        validator =
                            String.toInt >> Maybe.map DRec.fromInt

                        drec =
                            BaseTypes.basevalues
                                |> DRec.setWith BaseTypes.Inty validator "3.1"
                    in
                    Expect.all
                        [ DRec.hasValue BaseTypes.Inty >> Expect.equal False
                        , DRec.errorMessages >> Expect.equal (Just "Validation failed, field: inty")
                        , DRec.fieldBuffer BaseTypes.Inty >> Expect.equal (Just "3.1")
                        , DRec.isValid >> Expect.equal False
                        ]
                        drec
            , test "failed Float assignment" <|
                \_ ->
                    let
                        validator =
                            String.toFloat >> Maybe.map DRec.fromFloat

                        drec =
                            BaseTypes.basevalues
                                |> DRec.setWith BaseTypes.Floaty validator "31a"
                    in
                    Expect.all
                        [ DRec.hasValue BaseTypes.Floaty >> Expect.equal False
                        , DRec.errorMessages >> Expect.equal (Just "Validation failed, field: floaty")
                        , DRec.fieldBuffer BaseTypes.Floaty >> Expect.equal (Just "31a")
                        , DRec.isValid >> Expect.equal False
                        ]
                        drec
            , test "failed assignments" <|
                \_ ->
                    let
                        validatorInt =
                            String.toInt >> Maybe.map DRec.fromInt

                        validatorFloat =
                            String.toFloat >> Maybe.map DRec.fromFloat

                        drec =
                            BaseTypes.basevalues
                                |> DRec.setWith BaseTypes.Inty validatorInt "3.1"
                                |> DRec.setWith BaseTypes.Floaty validatorFloat "31a"
                    in
                    Expect.all
                        [ DRec.hasValue BaseTypes.Inty >> Expect.equal False
                        , DRec.hasValue BaseTypes.Floaty >> Expect.equal False
                        , DRec.errorMessages >> Expect.equal (Just "Validation failed, field: floaty | Validation failed, field: inty")
                        , DRec.fieldBuffer BaseTypes.Floaty >> Expect.equal (Just "31a")
                        , DRec.fieldBuffer BaseTypes.Inty >> Expect.equal (Just "3.1")
                        , DRec.isValid >> Expect.equal False
                        , DRec.fieldError BaseTypes.Floaty >> Expect.equal (Just "Validation failed, field: floaty")
                        , DRec.fieldError BaseTypes.Inty >> Expect.equal (Just "Validation failed, field: inty")
                        ]
                        drec
            ]
        ]
