module Model exposing
    ( Entry
    , Fields(..)
    , Model
    , emptyModel
    , newEntry
    )

import DRec exposing (DRec, DType(..), DValue(..), field, schema)


type Fields
    = Uid
    | Field
    | Visibility
    | Entries
    | Description -- entry record
    | Completed
    | Editing
    | Id


type alias Model =
    DRec Fields


type alias Entry =
    DRec Fields


entrySchema : Entry
entrySchema =
    DRec.init
        |> field Description DString
        |> field Completed DBool
        |> field Editing DBool
        |> field Id DInt


modelSchema : Model
modelSchema =
    DRec.init
        |> field Uid DInt
        |> field Field DString
        |> field Visibility DString
        |> field Entries (DList (VDRec <| schema entrySchema))


emptyModel : Model
emptyModel =
    modelSchema
        |> DRec.setList Entries DRec.fromDRec []
        |> DRec.setString Visibility "All"
        |> DRec.setString Field ""
        |> DRec.setInt Uid 0


newEntry : String -> Int -> Entry
newEntry desc id =
    entrySchema
        |> DRec.setString Description desc
        |> DRec.setBool Completed False
        |> DRec.setBool Editing False
        |> DRec.setInt Id id
