module DRec
    exposing
        ( DError(..)
        , DField
        , DRec
        , DType(..)
        , DValue(..)
        , empty
        , field
        , fromBool
        , fromFloat
        , fromInt
        , fromJson
        , fromMaybe
        , fromObject
        , fromString
        , get
        , hasSchema
        , isEmpty
        , set
        , toBool
        , toFloat
        , toInt
        , toJson
        , toMaybe
        , toObject
        , toString
        )

{-| Elm `Dict` based record with field name and type validation and automatic
decoding from and to JSON (`Json.Encode.Value`).


# Build

@docs DType, DValue, DRec, DField, DError, empty, field, set


# Query

@docs get, hasSchema, isEmpty


# Decode

@docs fromBool, fromFloat, fromInt, fromJson, fromMaybe, fromString

Decode from Elm types.


# Encode

@docs toBool, toFloat, toInt, toJson, toMaybe, toString

Encode to Elm types.


# JSON interop

@docs fromObject, toObject

-}

import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Encode


-- BUILD


{-| `DRec` schema types.
-}
type DType
    = DNever
    | DBool
    | DFloat
    | DInt
    | DJson
    | DMaybe DValue
    | DString


{-| Sub-type for scalar values.
-}
type DValue
    = VNil
    | VBool
    | VFloat
    | VInt
    | VJson
    | VString


{-| A record with schema.
-}
type DRec
    = DRec
        { schema : Dict String DType
        , store : Dict String DField
        }


{-| `DRec` field name and `DType` mapping, see `field`.
-}
type DField
    = DBool_ Bool
    | DFloat_ Float
    | DInt_ Int
    | DJson_ Json.Encode.Value
    | DMaybe_ (Maybe DField)
    | DString_ String


{-| @private
-}
fieldType : DField -> DType
fieldType dfield =
    case dfield of
        DBool_ _ ->
            DBool

        DFloat_ _ ->
            DFloat

        DInt_ _ ->
            DInt

        DJson_ _ ->
            DJson

        DMaybe_ mf ->
            case mf of
                Nothing ->
                    DMaybe VNil

                Just f ->
                    case fieldType f of
                        DBool ->
                            DMaybe VBool

                        DFloat ->
                            DMaybe VFloat

                        DInt ->
                            DMaybe VInt

                        DJson ->
                            DMaybe VJson

                        DString ->
                            DMaybe VString

                        _ ->
                            DNever

        DString_ _ ->
            DString


{-| Error tags
-}
type DError
    = DecodingFailed String
    | DuplicateField String
    | MissingField String
    | NoSchema
    | TypeMismatch String


{-| Create an empty `DRec`.
-}
empty : Result DError DRec
empty =
    DRec
        { schema = Dict.empty
        , store = Dict.empty
        }
        |> Ok


{-| Define `DRec` schema when initializing your application's model member.

    type alias Model =
        { user : DRec
        }

    init : Model
    init =
        { user =
            DRec.empty
                |> DRec.field "id" DInt
                |> DRec.field "email" DString
                |> DRec.field "name" DString
                |> DRec.field "token" DMaybe VString
        }

-}
field : String -> DType -> Result DError DRec -> Result DError DRec
field field dtype rr =
    case rr of
        Err x ->
            Err x

        Ok (DRec r) ->
            case
                Dict.get field r.schema
            of
                Nothing ->
                    DRec { r | schema = Dict.insert field dtype r.schema }
                        |> Ok

                Just _ ->
                    field
                        |> DuplicateField
                        |> Err


{-| Set a value for specified `DRec` field.

    update : Msg -> Model -> (Model, Cmd Msg)
    update msg model =
        Email str ->
            ( { model | user = DRec.set "email" DRec.fromString str model.user }
            , Cmd.none
            )

-}
set : String -> (a -> DField) -> a -> Result DError DRec -> Result DError DRec
set field toValue value rr =
    case rr of
        Err x ->
            Err x

        Ok (DRec r) ->
            case
                Dict.get field r.schema
            of
                Nothing ->
                    field
                        |> MissingField
                        |> Err

                Just dt ->
                    if fieldType (toValue value) == dt then
                        DRec { r | store = Dict.insert field (toValue value) r.store }
                            |> Ok
                    else
                        (Basics.toString (toValue value) ++ " /= " ++ Basics.toString dt)
                            |> TypeMismatch
                            |> Err



-- QUERY


{-| For a valid field defined in schema return a value/type mapping.
-}
get : String -> Result DError DRec -> Result DError DField
get field rr =
    case rr of
        Err x ->
            Err x

        Ok (DRec r) ->
            case
                Dict.get field r.store
            of
                Nothing ->
                    field
                        |> MissingField
                        |> Err

                Just dfield ->
                    Ok dfield


{-| Check if a schema has been specified.
-}
hasSchema : Result DError DRec -> Bool
hasSchema rr =
    case rr of
        Err _ ->
            False

        Ok (DRec r) ->
            Dict.isEmpty r.schema


{-| Check is specified `DRec` contains data.
-}
isEmpty : Result DError DRec -> Bool
isEmpty rr =
    case rr of
        Err _ ->
            False

        Ok (DRec r) ->
            Dict.isEmpty r.store



-- ENCODE


{-| -}
fromBool : Bool -> DField
fromBool v =
    DBool_ v


{-| -}
fromFloat : Float -> DField
fromFloat v =
    DFloat_ v


{-| -}
fromInt : Int -> DField
fromInt v =
    DInt_ v


{-| -}
fromJson : Json.Encode.Value -> DField
fromJson v =
    DJson_ v


{-| -}
fromMaybe : (a -> DField) -> Maybe a -> DField
fromMaybe f mv =
    case mv of
        Nothing ->
            DMaybe_ Nothing

        Just v ->
            DMaybe_ (Just (f v))


{-| -}
fromString : String -> DField
fromString v =
    DString_ v



-- DECODE


{-| -}
toBool : Result DError DField -> Result DError Bool
toBool rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DBool_ v ->
                    Ok v

                _ ->
                    "toBool"
                        |> TypeMismatch
                        |> Err


{-| -}
toFloat : Result DError DField -> Result DError Float
toFloat rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DFloat_ v ->
                    Ok v

                _ ->
                    "toFloat"
                        |> TypeMismatch
                        |> Err


{-| -}
toInt : Result DError DField -> Result DError Int
toInt rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DInt_ v ->
                    Ok v

                _ ->
                    "toInt"
                        |> TypeMismatch
                        |> Err


{-| -}
toJson : Result DError DField -> Result DError Json.Encode.Value
toJson rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DJson_ v ->
                    Ok v

                _ ->
                    "toJson"
                        |> TypeMismatch
                        |> Err


{-| Convert from a `DField` of `DType` 'DMaybe' to `Maybe a`.

    rec : DRec
    rec =
        DRec.empty
            |> DRec.field "token" DMaybe VString

    update : Maybe String -> DRec -> DRec
    update mv drec =
        DRec.set "token" (DRec.fromMaybe DRec.fromString) mv drec

    token : DRec -> Maybe String
    token drec =
        DRec.get "token" drec
            |> DRec.toMaybe (DRec.toString >> Result.toMaybe)

-}
toMaybe : (Result DError DField -> Maybe a) -> Result DError DField -> Result DError (Maybe a)
toMaybe toValue rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DMaybe_ m ->
                    case m of
                        Nothing ->
                            Nothing
                                |> Ok

                        Just df ->
                            toValue (Ok df)
                                |> Ok

                _ ->
                    "toMaybe"
                        |> TypeMismatch
                        |> Err


{-| -}
toString : Result DError DField -> Result DError String
toString rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DString_ v ->
                    Ok v

                _ ->
                    "toString"
                        |> TypeMismatch
                        |> Err



-- JSON interop


{-| @private
-}
fieldDecoder : String -> DType -> Decoder DField
fieldDecoder fname dtype =
    case dtype of
        DNever ->
            Json.Decode.fail "DNever is never decoded."

        DBool ->
            Json.Decode.field fname Json.Decode.bool
                |> Json.Decode.map fromBool

        DFloat ->
            Json.Decode.field fname Json.Decode.float
                |> Json.Decode.map fromFloat

        DInt ->
            Json.Decode.field fname Json.Decode.int
                |> Json.Decode.map fromInt

        DJson ->
            Json.Decode.field fname Json.Decode.value
                |> Json.Decode.map fromJson

        DMaybe dvalue ->
            Json.Decode.succeed True
                |> Json.Decode.map fromBool

        DString ->
            Json.Decode.field fname Json.Decode.string
                |> Json.Decode.map fromString


{-| @private
-}
drecDecoder : DRec -> Decoder DRec
drecDecoder (DRec r) =
    r.schema
        |> Dict.foldl (\fname dtype accum -> fieldDecoder fname dtype :: accum) []
        |> (\decoders ->
                let
                    first =
                        List.head decoders
                            |> Maybe.withDefault (Json.Decode.fail "DRec schema empty.")

                    rest =
                        List.drop 1 decoders
                in
                List.foldl (\dcdr accum -> Json.Decode.andThen Json.Decode.succeed dcdr) first decoders
           )
        |> (\decoder -> Json.Decode.Extra.dict2 Json.Decode.string decoder)
        |> Json.Decode.map (\d -> { r | store = d } |> DRec)


{-| Initialize `DRec` store/data by decoding specified JSON accordingly to `DRec` schema.
-}
fromObject : Result DError DRec -> Json.Encode.Value -> Result DError DRec
fromObject rr json =
    case rr of
        Err x ->
            Err x

        Ok drec ->
            if hasSchema rr then
                Json.Decode.decodeValue (drecDecoder drec) json
                    |> Result.mapError DecodingFailed
            else
                Err NoSchema


{-| Encode `DRec` into a JSON object accordingly to `DRec` schema.
-}
toObject : Result DError DRec -> Json.Encode.Value
toObject rr =
    case rr of
        Err x ->
            Json.Encode.object []

        Ok (DRec r) ->
            r.store
                |> Dict.foldl
                    (\field dfield accum ->
                        case dfield of
                            DBool_ b ->
                                ( field, Json.Encode.bool b ) :: accum

                            DFloat_ f ->
                                ( field, Json.Encode.float f ) :: accum

                            DInt_ i ->
                                ( field, Json.Encode.int i ) :: accum

                            DJson_ v ->
                                ( field, v ) :: accum

                            DMaybe_ mv ->
                                case mv of
                                    Nothing ->
                                        accum

                                    Just df ->
                                        case fieldType df of
                                            DMaybe VNil ->
                                                ( field, Json.Encode.null ) :: accum

                                            DMaybe VBool ->
                                                toBool (Ok df)
                                                    |> Result.map (\v -> ( field, Json.Encode.bool v ) :: accum)
                                                    |> Result.withDefault accum

                                            DMaybe VFloat ->
                                                toFloat (Ok df)
                                                    |> Result.map (\v -> ( field, Json.Encode.float v ) :: accum)
                                                    |> Result.withDefault accum

                                            DMaybe VInt ->
                                                toInt (Ok df)
                                                    |> Result.map (\v -> ( field, Json.Encode.int v ) :: accum)
                                                    |> Result.withDefault accum

                                            DMaybe VJson ->
                                                toJson (Ok df)
                                                    |> Result.map (\v -> ( field, v ) :: accum)
                                                    |> Result.withDefault accum

                                            DMaybe VString ->
                                                toString (Ok df)
                                                    |> Result.map (\v -> ( field, Json.Encode.string v ) :: accum)
                                                    |> Result.withDefault accum

                                            _ ->
                                                accum

                            DString_ s ->
                                ( field, Json.Encode.string s ) :: accum
                    )
                    []
                |> Json.Encode.object
