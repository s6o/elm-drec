module DRec exposing
    ( DType(..), DValue(..), DRec, DSchema, DField, DError(..), init, initWith, field
    , clear, setWith
    , setArray, setBool, setChar, setCharCode, setDRec, setFloat, setInt, setJson, setList, setMaybe, setString
    , decoder, decodeValue, decodeString, encoder
    , errorMessages, fieldBuffer, fieldError, fieldNames, get, hasSchema, hasValue, isEmpty, isValid, isValidWith, schema
    , fromArray, fromBool, fromChar, fromCharCode, fromDRec, fromFloat, fromInt, fromJson, fromList, fromMaybe, fromString
    , toArray, toBool, toChar, toCharCode, toDRec, toFloat, toInt, toJson, toList, toMaybe, toString
    )

{-| Elm `Dict` based record with field name and type validation and automatic
decoding from and to JSON.


# Build


## Schema

A schema is a similar construct to a regular Elm record definition. It describes
the member fields and their types.

A schema definitions usually start with the `init` or `initWith` function,
followed by several `field` functions piped after each other.

@docs DType, DValue, DRec, DSchema, DField, DError, init, initWith, field


## Values

`DRec a` value management provides an input buffer, input buffer validation and
validation related error management, in addition to the traditional storage of
values in a record's member fields.

Every time a value is set it goes through an input buffer, buffer validation,
error/success chain.

@docs clear, setWith


### Convenience functions

These functions wrap `setWith` with a type reflected in their name. The first
argument, as with `setWith` is the ADT specified for `DRec a`'s field names.

@docs setArray, setBool, setChar, setCharCode, setDRec, setFloat, setInt, setJson, setList, setMaybe, setString


## JSON interop

Create decoders and encoders based on the defined schema.

@docs decoder, decodeValue, decodeString, encoder


# Query

Helper functions to query the state and values of a `DRec a` and of its member fields.

@docs errorMessages, fieldBuffer, fieldError, fieldNames, get, hasSchema, hasValue, isEmpty, isValid, isValidWith, schema


# Decode

Decode from Elm base types to `DRec a`, as you would decode from a JSON string
to an Elm record/types.

@docs fromArray, fromBool, fromChar, fromCharCode, fromDRec, fromFloat, fromInt, fromJson, fromList, fromMaybe, fromString


# Encode

Encode to Elm base types from a `DRec a`, as you would encode to a JSON string
from an Elm record/types.

@docs toArray, toBool, toChar, toCharCode, toDRec, toFloat, toInt, toJson, toList, toMaybe, toString

-}

import Array exposing (Array)
import Char exposing (Char)
import Dict exposing (Dict)
import Json.Decode exposing (Decoder)
import Json.Encode



-- SCHEMA


{-| `DRec a` schema types.
-}
type DType
    = DNever
    | DArray DValue
    | DBool
    | DChar
    | DDRec DSchema
    | DFloat
    | DInt
    | DJson
    | DList DValue
    | DMaybe DValue
    | DString


{-| Sub-type for container types.
-}
type DValue
    = VBool
    | VChar
    | VDRec DSchema
    | VFloat
    | VInt
    | VJson
    | VString


{-| A record with schema, storge and input buffers.
-}
type DRec a
    = DRec
        { buffers : Dict String String
        , errors : Dict String DError
        , fields : List a
        , schema : Dict String DType
        , sfields : List String
        , store : Dict String (DField a)
        , toField : a -> String
        }


{-| `DRec a` field name and `DType` mapping, see `field`.
-}
type DField a
    = DArray_ (Array (DField a))
    | DBool_ Bool
    | DChar_ Char
    | DDRec_ (DRec a)
    | DFloat_ Float
    | DInt_ Int
    | DJson_ Json.Encode.Value
    | DList_ (List (DField a))
    | DMaybe_ (Maybe (DField a))
    | DString_ String


{-| `DRec a` schema.
-}
type DSchema
    = DSchema ( List String, Dict String DType )


{-| @private
-}
fieldType : String -> DField a -> DRec a -> DType
fieldType fname dfield (DRec r) =
    let
        schemaType =
            Dict.get fname r.schema
                |> Maybe.withDefault DNever
    in
    case dfield of
        DArray_ dfa ->
            case Array.get 0 dfa of
                Nothing ->
                    schemaType

                Just df ->
                    fieldSubType fname df (DRec r)
                        |> Maybe.map DArray
                        |> Maybe.withDefault schemaType

        DBool_ _ ->
            DBool

        DChar_ _ ->
            DChar

        DDRec_ (DRec sr) ->
            DDRec (DSchema ( sr.sfields, sr.schema ))

        DFloat_ _ ->
            DFloat

        DInt_ _ ->
            DInt

        DJson_ _ ->
            DJson

        DList_ dfl ->
            case List.head dfl of
                Nothing ->
                    schemaType

                Just df ->
                    fieldSubType fname df (DRec r)
                        |> Maybe.map DList
                        |> Maybe.withDefault schemaType

        DMaybe_ mf ->
            case mf of
                Nothing ->
                    schemaType

                Just f ->
                    fieldSubType fname f (DRec r)
                        |> Maybe.map DMaybe
                        |> Maybe.withDefault schemaType

        DString_ _ ->
            DString


{-| @private
-}
fieldSubType : String -> DField a -> DRec a -> Maybe DValue
fieldSubType fname dfield drec =
    case fieldType fname dfield drec of
        DBool ->
            Just VBool

        DChar ->
            Just VChar

        DDRec dschema ->
            Just <| VDRec dschema

        DFloat ->
            Just VFloat

        DInt ->
            Just VInt

        DJson ->
            Just VJson

        DString ->
            Just VString

        _ ->
            Nothing


{-| Error tags
-}
type DError
    = DecodingFailed String
    | DuplicateField String
    | InvalidSchemaType String
    | MissingValue String
    | NoSchema
    | TypeMismatch String
    | UknownField String
    | ValidationFailed String


{-| Initialize a `DRec a` with an ADT for fields and with the default ADT to `String` function.

The default ADT to `String` conversion is from 'camelCase' to 'snake\_case'. To
customize the conversion use `initWith`.

-}
init : DRec a
init =
    initWith toSnakeCase


{-| Initialize a `DRec a` with a custom ADT to `String` function.
-}
initWith : (a -> String) -> DRec a
initWith toField =
    DRec
        { buffers = Dict.empty
        , errors = Dict.empty
        , fields = []
        , schema = Dict.empty
        , sfields = []
        , store = Dict.empty
        , toField = toField
        }


{-| @private
-}
toSnakeCase : a -> String
toSnakeCase adt =
    Debug.toString adt
        |> String.foldl
            (\c accum ->
                if List.isEmpty accum then
                    Char.toLower c :: accum

                else if Char.isUpper c then
                    Char.toLower c :: ('_' :: accum)

                else
                    c :: accum
            )
            []
        |> List.reverse
        |> String.fromList


{-| Define `DRec a` schema when initializing your application's model member.

    module User exposing (UserField(..), init)

    import DRec exposing (DRec, DType(..), DValue(..))

    type UserField
        = Id
        | Email
        | Name
        | Token

    init : DRec UserField
    init =
        DRec.init
            |> DRec.field Id DInt
            |> DRec.field Email DString
            |> DRec.field Name DString
            |> DRec.field Token (DMaybe VString)

    -- ...

    module Model exposing (Model, Msg(..), init)

    import DRec exposing (DRec)
    import User exposing(..)

    -- ...

    type alias Model =
        { user : DRec UserField
        }

    init : Model
    init =
        { user = User.init
        }

-}
field : a -> DType -> DRec a -> DRec a
field adt dtype (DRec r) =
    let
        rfield =
            r.toField adt
    in
    case
        Dict.get rfield r.schema
    of
        Nothing ->
            DRec
                { r
                    | fields = r.fields ++ [ adt ]
                    , sfields = r.sfields ++ [ r.toField adt ]
                    , schema = Dict.insert rfield dtype r.schema
                }

        Just _ ->
            rfield
                |> DuplicateField
                |> (\derror -> { r | errors = Dict.insert rfield derror r.errors })
                |> DRec



-- VALUES


{-| Remove all data (including input buffers and errors) from `DRec a`, schema is not affected.
-}
clear : DRec a -> DRec a
clear (DRec r) =
    DRec { r | buffers = Dict.empty, errors = Dict.empty, store = Dict.empty }


{-| Set an `Array b` value for specified `DRec a` field.
-}
setArray : a -> (b -> DField a) -> Array b -> DRec a -> DRec a
setArray fld fromValue vals drec =
    setWith fld (fromArray fromValue >> Just) vals drec


{-| Set a `Bool` value for specified `DRec a` field.
-}
setBool : a -> Bool -> DRec a -> DRec a
setBool fld val drec =
    setWith fld (fromBool >> Just) val drec


{-| Set a `Char` value for specified `DRec a` field.
-}
setChar : a -> Char -> DRec a -> DRec a
setChar fld val drec =
    setWith fld (fromChar >> Just) val drec


{-| Set a `Int` value via `Char.fromCode` for specified `DRec a` field.
-}
setCharCode : a -> Int -> DRec a -> DRec a
setCharCode fld val drec =
    setWith fld (fromCharCode >> Just) val drec


{-| Set a sub `DRec a` for spcified `DRec a` field.
-}
setDRec : a -> DRec a -> DRec a -> DRec a
setDRec fld val drec =
    setWith fld (fromDRec >> Just) val drec


{-| Set a `Float` value for specified `DRec a` field.
-}
setFloat : a -> Float -> DRec a -> DRec a
setFloat fld val drec =
    setWith fld (fromFloat >> Just) val drec


{-| Set a `Int` value for specified `DRec a` field.
-}
setInt : a -> Int -> DRec a -> DRec a
setInt fld val drec =
    setWith fld (fromInt >> Just) val drec


{-| Set a `Json.Encode.Value` value for specified `DRec a` field.
-}
setJson : a -> Json.Encode.Value -> DRec a -> DRec a
setJson fld val drec =
    setWith fld (fromJson >> Just) val drec


{-| Set an `List b` value for specified `DRec a` field.
-}
setList : a -> (b -> DField a) -> List b -> DRec a -> DRec a
setList fld fromValue vals drec =
    setWith fld (fromList fromValue >> Just) vals drec


{-| Set an `Maybe b` value for specified `DRec a` field.
-}
setMaybe : a -> (b -> DField a) -> Maybe b -> DRec a -> DRec a
setMaybe fld fromValue vals drec =
    setWith fld (fromMaybe fromValue >> Just) vals drec


{-| Set a `String` value for specified `DRec a` field.
-}
setString : a -> String -> DRec a -> DRec a
setString fld val drec =
    setWith fld (fromString >> Just) val drec


{-| Set a value for specified `DRec a` field with a custom value conversion/validation.

In case of a conversion/validation error - the function given as the 2nd argument,
must return `Nothing`, the value is then retained in an internal input buffer and
an error is set for the specified field.

For quering the error and input buffer use `fieldError` and `fieldBuffer` respectively.

    import DRec exposing (DRec)
    import User exposing (..)

    type Msg
        = UserEmail String
        | UserName String
        | UserToken (Maybe String)

    -- ...
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            UserEmail email ->
                ( { model | user = DRec.setWith User.Email (fromString >> Just) email model.user }
                , Cmd.none
                )

            UserName uname ->
                -- same as above via the convenience wrapper function
                ( { model | user = DRec.setString User.Name uname model.user }
                , Cmd.none
                )

            UserToken mtkn ->
                ( { model | user = DRec.setWith User.Token (DRec.fromMaybe DRec.fromString >> Just) mtkn model.user }
                , Cmd.none
                )

-}
setWith : a -> (b -> Maybe (DField a)) -> b -> DRec a -> DRec a
setWith adt toValue value (DRec r) =
    setWithP (r.toField adt) toValue value (DRec r)


{-| @private
-}
setWithP : String -> (b -> Maybe (DField a)) -> b -> DRec a -> DRec a
setWithP fld toValue val (DRec r) =
    let
        setError bufferFlag de =
            case bufferFlag of
                False ->
                    { r | errors = Dict.insert fld de r.errors }

                True ->
                    let
                        bufferValue =
                            Debug.toString val
                                -- Debug.toString adds quotes which we don't want
                                |> (String.dropLeft 1 >> String.dropRight 1)
                    in
                    { r
                        | buffers = Dict.insert fld bufferValue r.buffers
                        , errors = Dict.insert fld de r.errors
                    }
    in
    case
        Dict.get fld r.schema
    of
        Nothing ->
            fld
                |> UknownField
                |> setError False
                |> DRec

        Just dt ->
            toValue val
                |> Maybe.map
                    (\dfield ->
                        if fieldType fld dfield (DRec r) == dt then
                            DRec
                                { r
                                    | buffers = Dict.remove fld r.buffers
                                    , errors = Dict.remove fld r.errors
                                    , store = Dict.insert fld dfield r.store
                                }

                        else
                            (Debug.toString (fieldType fld dfield (DRec r)) ++ " /= " ++ Debug.toString dt)
                                |> TypeMismatch
                                |> setError True
                                |> DRec
                    )
                |> Maybe.withDefault
                    (ValidationFailed fld
                        |> setError True
                        |> DRec
                    )



-- QUERY


{-| @private
-}
derrorString : DError -> String
derrorString derror =
    case derror of
        DecodingFailed msg ->
            "Decoding failed: " ++ msg

        DuplicateField msg ->
            "Duplicate field: " ++ msg

        InvalidSchemaType msg ->
            "Invalid schema type: " ++ msg

        MissingValue msg ->
            "Missing value: " ++ msg

        NoSchema ->
            "No schema."

        TypeMismatch msg ->
            "Type mismatch: " ++ msg

        UknownField msg ->
            "Unknown field: " ++ msg

        ValidationFailed msg ->
            "Validation failed, field: " ++ msg


{-| Get all errors messages as a single string.
-}
errorMessages : DRec a -> Maybe String
errorMessages (DRec r) =
    if Dict.isEmpty r.errors then
        Nothing

    else
        r.errors
            |> Dict.foldl
                (\fld derror accum ->
                    if String.isEmpty accum then
                        accum ++ derrorString derror

                    else
                        accum ++ (" | " ++ derrorString derror)
                )
                ""
            |> Just


{-| Query field's input buffer.
-}
fieldBuffer : a -> DRec a -> Maybe String
fieldBuffer adt (DRec r) =
    Dict.get (r.toField adt) r.buffers


{-| Query error message for a field.
-}
fieldError : a -> DRec a -> Maybe String
fieldError adt (DRec r) =
    Dict.get (r.toField adt) r.errors
        |> Maybe.map derrorString


{-| Get field names in the order they were defined.
-}
fieldNames : DRec a -> List a
fieldNames (DRec r) =
    r.fields


{-| For a valid field defined in schema return a value/type mapping.
-}
get : a -> DRec a -> Result DError (DField a)
get adt (DRec r) =
    let
        {-
           -- This fails, for sub-record, making the initWith rather useless and
           -- the default init does not work - WTF?!
           fld =
               r.toField adt
        -}
        {-
           -- being explicit/hard-coded works
        -}
        fld =
            toSnakeCase adt
    in
    case
        Dict.get fld r.schema
    of
        Nothing ->
            fld
                |> UknownField
                |> Err

        Just _ ->
            case
                Dict.get fld r.store
            of
                Nothing ->
                    fld
                        |> MissingValue
                        |> Err

                Just dfield ->
                    Ok dfield


{-| Check if a schema has been specified.
-}
hasSchema : DRec a -> Bool
hasSchema (DRec r) =
    not <| Dict.isEmpty r.schema


{-| Check if specified field has a valid value.

A valid value is considered to be present if no input buffer for the field is
set and the field itself actually contains a valid value.

In case of `DMaybe` a `Nothing` is considered a valid existing value.

-}
hasValue : a -> DRec a -> Bool
hasValue fld drec =
    fieldBuffer fld drec
        |> Maybe.map (\_ -> False)
        |> Maybe.withDefault
            (get fld drec
                |> Result.map (\_ -> True)
                |> Result.withDefault False
            )


{-| Check is specified `DRec a` contains data.
-}
isEmpty : DRec a -> Bool
isEmpty (DRec r) =
    Dict.isEmpty r.store


{-| Check if a record is valid: no errors and `hasValue` returns `True` for every field.
-}
isValid : DRec a -> Bool
isValid (DRec r) =
    isValidWith r.fields (DRec r)


{-| Check if a record is valid for specified fields: no errors and `hasValue`
returns `True` for every listed field.

This is useful for cases where you might want filter out the primary key of a
database table that will be handled automatically by the database and thus,
will not need to be set in the client.

-}
isValidWith : List a -> DRec a -> Bool
isValidWith fields (DRec r) =
    fields
        |> List.foldl
            (\fname accum -> hasValue fname (DRec r) && accum)
            (Dict.isEmpty r.errors)


{-| Query `DRec a` schema, for use in another `DRec a`.

    type AddressField
        = SteetName
        | BuildingNumber
        | SubNumber

    type PersonField
        = Name
        | Address

    address : DRec AddressField
    address =
        DRec.init
            |> DRec.field StreetName DString
            |> DRec.field BuildingNumber DInt
            |> DRec.field SubNumber (DMaybe DInt)

    person : DRec PersonField
    person =
        DRec.init
            |> DRec.field Name DString
            |> DRec.field Address (DDRec <| DRec.schema address)

-}
schema : DRec a -> DSchema
schema (DRec r) =
    DSchema ( r.sfields, r.schema )



-- ENCODE


{-| Convert from `Array b` to DField.
-}
fromArray : (b -> DField a) -> Array b -> DField a
fromArray f v =
    Array.map f v
        |> DArray_


{-| Convert from `Bool` to `DField a`.
-}
fromBool : Bool -> DField a
fromBool v =
    DBool_ v


{-| Convert from `Char` to `DField a`.
-}
fromChar : Char -> DField a
fromChar v =
    DChar_ v


{-| Convert from `Int` via `Char.fromCode` to `DField a`.
-}
fromCharCode : Int -> DField a
fromCharCode v =
    Char.fromCode v |> DChar_


{-| Convert from `DRec a` to `DField a`.
-}
fromDRec : DRec a -> DField a
fromDRec v =
    DDRec_ v


{-| Convert from `Float` to `DField a`.
-}
fromFloat : Float -> DField a
fromFloat v =
    DFloat_ v


{-| Convert from `Int` to `DField a`.
-}
fromInt : Int -> DField a
fromInt v =
    DInt_ v


{-| Convert from `Json.Encode.Value` to `DField a`.
-}
fromJson : Json.Encode.Value -> DField a
fromJson v =
    DJson_ v


{-| Convert from `List b` to DField.
-}
fromList : (b -> DField a) -> List b -> DField a
fromList f v =
    List.map f v
        |> DList_


{-| Convert from `Maybe b` to `DField a`.
-}
fromMaybe : (b -> DField a) -> Maybe b -> DField a
fromMaybe f mv =
    case mv of
        Nothing ->
            DMaybe_ Nothing

        Just v ->
            DMaybe_ (Just (f v))


{-| Convert from `String` to `DField a`.
-}
fromString : String -> DField a
fromString v =
    DString_ v



-- DECODE


{-| Convert from `DField a` to `Array b`
-}
toArray : (Result DError (DField a) -> Result DError b) -> Result DError (DField a) -> Result DError (Array b)
toArray toValue rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DArray_ dfarray ->
                    dfarray
                        |> Array.foldr
                            (\df accum ->
                                toValue (Ok df)
                                    |> Result.map (\v -> v :: accum)
                                    |> Result.withDefault accum
                            )
                            []
                        |> Array.fromList
                        |> Ok

                _ ->
                    ("DRec.toArray " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `Bool`.
-}
toBool : Result DError (DField a) -> Result DError Bool
toBool rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DBool_ v ->
                    Ok v

                DMaybe_ (Just (DBool_ v)) ->
                    Ok v

                _ ->
                    ("DRec.toBool " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `Char`.
-}
toChar : Result DError (DField a) -> Result DError Char
toChar rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DChar_ v ->
                    Ok v

                DMaybe_ (Just (DChar_ v)) ->
                    Ok v

                _ ->
                    ("DRec.toChar " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `Int` via `Char.toCode`.
-}
toCharCode : Result DError (DField a) -> Result DError Int
toCharCode rf =
    toChar rf
        |> Result.map Char.toCode
        |> Result.mapError (\_ -> TypeMismatch "DRec.toCharCode")


{-| Convert from `DField a` to `DRec a`.
-}
toDRec : Result DError (DField a) -> Result DError (DRec a)
toDRec rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DDRec_ v ->
                    Ok v

                DMaybe_ (Just (DDRec_ v)) ->
                    Ok v

                _ ->
                    ("DRec.toDRec " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `Float`.
-}
toFloat : Result DError (DField a) -> Result DError Float
toFloat rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DFloat_ v ->
                    Ok v

                DMaybe_ (Just (DFloat_ v)) ->
                    Ok v

                _ ->
                    ("DRec.toFloat " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `Int`.
-}
toInt : Result DError (DField a) -> Result DError Int
toInt rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DInt_ v ->
                    Ok v

                DMaybe_ (Just (DInt_ v)) ->
                    Ok v

                _ ->
                    ("DRec.toInt " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `Json.Encode.Value`.
-}
toJson : Result DError (DField a) -> Result DError Json.Encode.Value
toJson rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DJson_ v ->
                    Ok v

                DMaybe_ (Just (DJson_ v)) ->
                    Ok v

                _ ->
                    ("DRec.toJson " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `List b`.
-}
toList : (Result DError (DField a) -> Result DError b) -> Result DError (DField a) -> Result DError (List b)
toList toValue rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DList_ dfl ->
                    dfl
                        |> List.foldr
                            (\df accum ->
                                toValue (Ok df)
                                    |> Result.map (\v -> v :: accum)
                                    |> Result.withDefault accum
                            )
                            []
                        |> Ok

                _ ->
                    ("DRec.toList " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from a `DField a` of `DType` `DMaybe (DField a)` to `Maybe b`.

    type Field
        = Token

    rec : DRec Field
    rec =
        DRec.init
            |> DRec.field Token (DMaybe VString)

    update : Maybe String -> DRec Field -> DRec Field
    update mv drec =
        DRec.setWith Token (DRec.fromMaybe DRec.fromString >> Just) mv drec

    token : DRec Field -> Maybe String
    token drec =
        DRec.get Token drec
            |> DRec.toMaybe DRec.toString
            |> Maybe.withDefault Nothing

-}
toMaybe : (Result DError (DField a) -> Result DError b) -> Result DError (DField a) -> Result DError (Maybe b)
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
                                |> Result.map Just
                                |> Result.withDefault Nothing
                                |> Ok

                _ ->
                    ("DRec.toMaybe " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err


{-| Convert from `DField a` to `String`.
-}
toString : Result DError (DField a) -> Result DError String
toString rf =
    case rf of
        Err x ->
            Err x

        Ok dfield ->
            case dfield of
                DString_ v ->
                    Ok v

                DMaybe_ (Just (DString_ v)) ->
                    Ok v

                _ ->
                    ("DRec.toString " ++ Debug.toString dfield)
                        |> TypeMismatch
                        |> Err



-- JSON interop


{-| @private
-}
arrayDecoder : String -> DRec a -> (b -> DField a) -> Decoder (Array b) -> Decoder (DRec a)
arrayDecoder fname drec toItem adecoder =
    adecoder
        |> Json.Decode.field fname
        |> Json.Decode.map (\v -> setWithP fname (fromArray toItem >> Just) v drec)


{-| @private
-}
listDecoder : String -> DRec a -> (b -> DField a) -> Decoder (List b) -> Decoder (DRec a)
listDecoder fname drec toItem ldecoder =
    ldecoder
        |> Json.Decode.field fname
        |> Json.Decode.map (\v -> setWithP fname (fromList toItem >> Just) v drec)


{-| @private
-}
maybeDecoder : String -> DRec a -> (b -> DField a) -> Decoder (Maybe b) -> Decoder (DRec a)
maybeDecoder fname drec toItem mdecoder =
    mdecoder
        |> Json.Decode.map (\mv -> setWithP fname (fromMaybe toItem >> Just) mv drec)


{-| @private
-}
fieldDecoder : String -> DType -> DRec a -> Decoder (DRec a)
fieldDecoder fname dtype drec =
    case dtype of
        DNever ->
            Json.Decode.fail "DNever is never decoded."

        DArray dvalue ->
            case dvalue of
                VBool ->
                    Json.Decode.array Json.Decode.bool
                        |> arrayDecoder fname drec fromBool

                VChar ->
                    Json.Decode.array (Json.Decode.map Char.fromCode Json.Decode.int)
                        |> arrayDecoder fname drec fromChar

                VDRec (DSchema ( forder, stypes )) ->
                    let
                        subRec =
                            DRec
                                { buffers = Dict.empty
                                , errors = Dict.empty
                                , fields = []
                                , sfields = forder
                                , schema = stypes
                                , store = Dict.empty
                                , toField = Debug.toString
                                }
                    in
                    Json.Decode.array (subDecoder subRec)
                        |> arrayDecoder fname drec fromDRec

                VFloat ->
                    Json.Decode.array Json.Decode.float
                        |> arrayDecoder fname drec fromFloat

                VInt ->
                    Json.Decode.array Json.Decode.int
                        |> arrayDecoder fname drec fromInt

                VJson ->
                    Json.Decode.array Json.Decode.value
                        |> arrayDecoder fname drec fromJson

                VString ->
                    Json.Decode.array Json.Decode.string
                        |> arrayDecoder fname drec fromString

        DBool ->
            Json.Decode.field fname Json.Decode.bool
                |> Json.Decode.map (\v -> setWithP fname (fromBool >> Just) v drec)

        DChar ->
            Json.Decode.field fname (Json.Decode.map Char.fromCode Json.Decode.int)
                |> Json.Decode.map (\v -> setWithP fname (fromChar >> Just) v drec)

        DDRec (DSchema ( forder, stypes )) ->
            let
                subRec =
                    DRec
                        { buffers = Dict.empty
                        , errors = Dict.empty
                        , fields = []
                        , sfields = forder
                        , schema = stypes
                        , store = Dict.empty
                        , toField = Debug.toString
                        }
            in
            Json.Decode.field fname (subDecoder subRec)
                |> Json.Decode.map (\v -> setWithP fname (fromDRec >> Just) v drec)

        DFloat ->
            Json.Decode.field fname Json.Decode.float
                |> Json.Decode.map (\v -> setWithP fname (fromFloat >> Just) v drec)

        DInt ->
            Json.Decode.field fname Json.Decode.int
                |> Json.Decode.map (\v -> setWithP fname (fromInt >> Just) v drec)

        DJson ->
            Json.Decode.field fname Json.Decode.value
                |> Json.Decode.map (\v -> setWithP fname (fromJson >> Just) v drec)

        DList dvalue ->
            case dvalue of
                VBool ->
                    Json.Decode.list Json.Decode.bool
                        |> listDecoder fname drec fromBool

                VChar ->
                    Json.Decode.list (Json.Decode.map Char.fromCode Json.Decode.int)
                        |> listDecoder fname drec fromChar

                VDRec (DSchema ( forder, stypes )) ->
                    let
                        subRec =
                            DRec
                                { buffers = Dict.empty
                                , errors = Dict.empty
                                , fields = []
                                , sfields = forder
                                , schema = stypes
                                , store = Dict.empty
                                , toField = Debug.toString
                                }
                    in
                    Json.Decode.list (subDecoder subRec)
                        |> listDecoder fname drec fromDRec

                VFloat ->
                    Json.Decode.list Json.Decode.float
                        |> listDecoder fname drec fromFloat

                VInt ->
                    Json.Decode.list Json.Decode.int
                        |> listDecoder fname drec fromInt

                VJson ->
                    Json.Decode.list Json.Decode.value
                        |> listDecoder fname drec fromJson

                VString ->
                    Json.Decode.list Json.Decode.string
                        |> listDecoder fname drec fromString

        DMaybe dvalue ->
            case dvalue of
                VBool ->
                    Json.Decode.maybe (Json.Decode.field fname Json.Decode.bool)
                        |> maybeDecoder fname drec fromBool

                VChar ->
                    Json.Decode.maybe (Json.Decode.field fname (Json.Decode.map Char.fromCode Json.Decode.int))
                        |> maybeDecoder fname drec fromChar

                VDRec (DSchema ( forder, stypes )) ->
                    let
                        subRec =
                            DRec
                                { buffers = Dict.empty
                                , errors = Dict.empty
                                , fields = []
                                , sfields = forder
                                , schema = stypes
                                , store = Dict.empty
                                , toField = Debug.toString
                                }
                    in
                    Json.Decode.maybe (Json.Decode.field fname (subDecoder subRec))
                        |> maybeDecoder fname drec fromDRec

                VFloat ->
                    Json.Decode.maybe (Json.Decode.field fname Json.Decode.float)
                        |> maybeDecoder fname drec fromFloat

                VInt ->
                    Json.Decode.maybe (Json.Decode.field fname Json.Decode.int)
                        |> maybeDecoder fname drec fromInt

                VJson ->
                    Json.Decode.maybe (Json.Decode.field fname Json.Decode.value)
                        |> maybeDecoder fname drec fromJson

                VString ->
                    Json.Decode.maybe (Json.Decode.field fname Json.Decode.string)
                        |> maybeDecoder fname drec fromString

        DString ->
            Json.Decode.field fname Json.Decode.string
                |> Json.Decode.map (\v -> setWithP fname (fromString >> Just) v drec)


{-| @private
Aggregate `DRec a` member decoders.
-}
subDecoder : DRec a -> Decoder (DRec a)
subDecoder (DRec r) =
    r.sfields
        |> List.foldl
            (\fname accum ->
                Dict.get fname r.schema
                    |> Maybe.map (\dtype -> Json.Decode.andThen (fieldDecoder fname dtype) accum)
                    |> Maybe.withDefault accum
            )
            (DRec r |> Json.Decode.succeed)


{-| Create decoder for specified `DRec a`.
-}
decoder : DRec a -> Decoder (DRec a)
decoder (DRec r) =
    if Dict.isEmpty r.errors then
        subDecoder (DRec r)

    else
        errorMessages (DRec r)
            |> Maybe.map Json.Decode.fail
            |> Maybe.withDefault
                ("decoder logic failure, how do you got here?"
                    |> Json.Decode.fail
                )


{-| Initialize `DRec a` data by decoding specified JSON (`Json.Encode.Value`) accordingly to `DRec a` schema.
-}
decodeValue : DRec a -> Json.Encode.Value -> Result DError (DRec a)
decodeValue drec json =
    if hasSchema drec then
        Json.Decode.decodeValue (decoder drec) json
            |> Result.mapError (Json.Decode.errorToString >> DecodingFailed)

    else
        Err NoSchema


{-| Initialize `DRec a` data by decoding specified JSON string literal accordingly to `DRec a` schema.
-}
decodeString : DRec a -> String -> Result DError (DRec a)
decodeString drec json =
    if hasSchema drec then
        Json.Decode.decodeString (decoder drec) json
            |> Result.mapError (Json.Decode.errorToString >> DecodingFailed)

    else
        Err NoSchema


{-| Encode `DRec a` into a JSON object accordingly to `DRec a` schema.
-}
encoder : DRec a -> Json.Encode.Value
encoder (DRec r) =
    if Dict.isEmpty r.errors then
        subObject (DRec r)

    else
        Json.Encode.object []


{-| @private
Encode specified `DRec a`.
-}
subObject : DRec a -> Json.Encode.Value
subObject (DRec r) =
    r.sfields
        |> List.foldr
            (\fld accum ->
                Dict.get fld r.store
                    |> Maybe.map (objectField fld accum)
                    |> Maybe.withDefault accum
            )
            []
        |> Json.Encode.object


{-| @private
-}
objectField : String -> List ( String, Json.Encode.Value ) -> DField a -> List ( String, Json.Encode.Value )
objectField fld accum dfield =
    case dfield of
        DArray_ dfarray ->
            let
                valueArray =
                    dfarray
                        |> Array.foldr
                            (\df a ->
                                case objectField fld [] df |> List.head of
                                    Nothing ->
                                        a

                                    Just ( _, v ) ->
                                        v :: a
                            )
                            []
                        |> Array.fromList
            in
            ( fld, Json.Encode.array Basics.identity valueArray ) :: accum

        DBool_ b ->
            ( fld, Json.Encode.bool b ) :: accum

        DChar_ c ->
            ( fld, Char.toCode c |> Json.Encode.int ) :: accum

        DDRec_ sdrec ->
            ( fld, subObject sdrec ) :: accum

        DFloat_ f ->
            ( fld, Json.Encode.float f ) :: accum

        DInt_ i ->
            ( fld, Json.Encode.int i ) :: accum

        DJson_ v ->
            ( fld, v ) :: accum

        DList_ dfl ->
            let
                valueList =
                    dfl
                        |> List.foldr
                            (\df a ->
                                case objectField fld [] df |> List.head of
                                    Nothing ->
                                        a

                                    Just ( _, v ) ->
                                        v :: a
                            )
                            []
            in
            ( fld, Json.Encode.list Basics.identity valueList ) :: accum

        DMaybe_ mv ->
            case mv of
                Nothing ->
                    accum

                Just df ->
                    objectField fld accum df

        DString_ s ->
            ( fld, Json.Encode.string s ) :: accum
