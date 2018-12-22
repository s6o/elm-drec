module Tests exposing (suite)

import DRec
import Expect exposing (Expectation)
import Test exposing (..)
import User exposing (..)


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
            , test "Check User schema initialization" <|
                \_ ->
                    Expect.all
                        [ DRec.hasSchema >> Expect.equal True
                        , DRec.isEmpty >> Expect.equal True
                        , DRec.isValid >> Expect.equal False
                        , DRec.errorMessages >> Expect.equal Nothing
                        , DRec.fieldNames >> Expect.equal User.fieldNames
                        ]
                        User.init
            ]
        , describe "JSON interop"
            [ test "Decode User from JSON" <|
                \_ ->
                    Expect.all
                        [ (Result.map (DRec.get User.Id) >> Result.andThen DRec.toInt) >> Expect.equal (Ok 1)
                        , (Result.map (DRec.get User.Email) >> Result.andThen DRec.toString) >> Expect.equal (Ok "john.doe@lost.net")
                        , (Result.map (DRec.get User.Name) >> Result.andThen DRec.toString) >> Expect.equal (Ok "John Doe")
                        , (Result.map (DRec.get User.Token) >> Result.andThen (DRec.toMaybe DRec.toString)) >> Expect.equal (Ok (Just "abcdef01234"))
                        ]
                        (DRec.decodeString
                            User.init
                            User.johndoe
                        )
            ]
        ]
