module Main exposing (Entry, Model, Msg(..), emptyModel, infoFooter, init, main, newEntry, onEnter, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewInput, viewKeyedEntry, visibilitySwap)

{-| TodoMVC implemented in Elm with PostgREST, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

1.  Model - a full definition of the application's state
2.  Update - a way to step the application state forward
3.  View - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>

-}

import Browser
import Browser.Dom as Dom
import DRec exposing (DRec, DType(..), DValue(..), field, schema)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Json
import Json.Encode
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = \model -> { title = "Elm • TodoMVC • PostgREST", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


{-| We want to `setStorage` on every update, if the model has changed.
This function adds the setStorage command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model

        oldJson =
            DRec.encode model
                |> Json.Encode.encode 0

        newJson =
            DRec.encode newModel
                |> Json.Encode.encode 0
    in
    ( newModel
    , Cmd.batch
        [ if oldJson /= newJson then
            setStorage newModel

          else
            Cmd.none
        , cmds
        ]
    )


{-| Make a GET request to PostgREST service to load (initial) model.
-}
getStorage : Model -> Cmd Msg
getStorage model =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Accept" "application/vnd.pgrst.object+json"
            , Http.header "Accept" "application/json"
            ]
        , url = "http://localhost:8002/api/model"
        , body = Http.emptyBody
        , expect = Http.expectJson InitModel (DRec.decoder model)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Store model changes.
-}
setStorage : Model -> Cmd Msg
setStorage model =
    Http.request
        { method = "PATCH"
        , headers =
            [ Http.header "Accept" "application/vnd.pgrst.object+json"
            , Http.header "Prefer" "return=representation"
            ]
        , url = "http://localhost:8002/api/model"
        , body = Http.jsonBody (DRec.encode model)
        , expect = Http.expectJson (\_ -> NoOp) (DRec.decoder model)
        , timeout = Nothing
        , tracker = Nothing
        }



-- MODEL
-- The full application state of our todo app.


type Fields
    = Uid
    | Field
    | Visibility
    | Entries
    | Description -- entry record
    | Completed
    | Editing
    | Id


{-| DRec record definition for an `Entry`.
-}
entrySchema : DRec Fields
entrySchema =
    DRec.init
        |> field Description DString
        |> field Completed DBool
        |> field Editing DBool
        |> field Id DInt


{-| DRec record definition for the `Model`.
-}
modelSchema : DRec Fields
modelSchema =
    DRec.init
        |> field Uid DInt
        |> field Field DString
        |> field Visibility DString
        |> field Entries (DList (VDRec <| schema entrySchema))


type alias Model =
    DRec Fields


type alias Entry =
    DRec Fields


{-| Helper to access DRec based `Entry` and `Model` members.
-}
get :
    { entries : DRec Fields -> List (DRec Fields)
    , field : DRec Fields -> String
    , uid : DRec Fields -> Int
    , visibility : DRec Fields -> String
    , description : DRec Fields -> String
    , completed : DRec Fields -> Bool
    , editing : DRec Fields -> Bool
    , id : DRec Fields -> Int
    }
get =
    { entries = DRec.get Entries >> DRec.toList DRec.toDRec >> Result.withDefault []
    , field = DRec.get Field >> DRec.toString >> Result.withDefault ""
    , uid = DRec.get Uid >> DRec.toInt >> Result.withDefault 0
    , visibility = DRec.get Visibility >> DRec.toString >> Result.withDefault "All"
    , description = DRec.get Description >> DRec.toString >> Result.withDefault ""
    , completed = DRec.get Completed >> DRec.toBool >> Result.withDefault True
    , editing = DRec.get Editing >> DRec.toBool >> Result.withDefault False
    , id = DRec.get Id >> DRec.toInt >> Result.withDefault 0
    }


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


init : ( Model, Cmd Msg )
init =
    ( emptyModel
    , getStorage emptyModel
    )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg
    = NoOp
    | InitModel (Result Http.Error Model)
    | UpdateField String
    | EditingEntry Int Bool
    | UpdateEntry Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String



-- How we update our Model on a given Msg?


update : Msg -> Model -> ( Model, Cmd Msg )
update msg drec =
    case msg of
        NoOp ->
            ( drec, Cmd.none )

        InitModel (Err _) ->
            ( emptyModel
            , Cmd.none
            )

        InitModel (Ok model) ->
            ( model
            , Cmd.none
            )

        Add ->
            let
                newdrec =
                    drec
                        |> DRec.setInt Uid (get.uid drec + 1)
                        |> DRec.setString Field ""
                        |> DRec.setList Entries
                            DRec.fromDRec
                            (if String.isEmpty (get.field drec) then
                                get.entries drec

                             else
                                get.entries drec ++ [ newEntry (get.field drec) (get.uid drec) ]
                            )
            in
            ( newdrec
            , setStorage newdrec
            )

        UpdateField str ->
            ( drec
                |> DRec.setString Field str
            , Cmd.none
            )

        EditingEntry id isEditing ->
            let
                updateEntry t =
                    if get.id t == id then
                        DRec.setBool Editing isEditing t

                    else
                        t

                focus =
                    Dom.focus ("todo-" ++ String.fromInt id)
            in
            ( drec
                |> DRec.setList Entries DRec.fromDRec (List.map updateEntry (get.entries drec))
            , Task.attempt (\_ -> NoOp) focus
            )

        UpdateEntry id task ->
            let
                updateEntry t =
                    if get.id t == id then
                        DRec.setString Description task t

                    else
                        t
            in
            ( drec
                |> DRec.setList Entries DRec.fromDRec (List.map updateEntry (get.entries drec))
            , Cmd.none
            )

        Delete id ->
            ( drec
                |> DRec.setList Entries DRec.fromDRec (List.filter (\t -> get.id t /= id) (get.entries drec))
            , Cmd.none
            )

        DeleteComplete ->
            ( drec
                |> DRec.setList Entries DRec.fromDRec (List.filter (not << get.completed) (get.entries drec))
            , Cmd.none
            )

        Check id isCompleted ->
            let
                updateEntry t =
                    if get.id t == id then
                        DRec.setBool Completed isCompleted t

                    else
                        t
            in
            ( drec
                |> DRec.setList Entries DRec.fromDRec (List.map updateEntry (get.entries drec))
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateEntry t =
                    DRec.setBool Completed isCompleted t
            in
            ( drec
                |> DRec.setList Entries DRec.fromDRec (List.map updateEntry (get.entries drec))
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( drec
                |> DRec.setString Visibility visibility
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view drec =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ lazy viewInput (get.field drec)
            , lazy2 viewEntries (get.visibility drec) (get.entries drec)
            , lazy2 viewControls (get.visibility drec) (get.entries drec)
            ]
        , infoFooter
        ]


viewInput : String -> Html Msg
viewInput task =
    header
        [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , autofocus True
            , value task
            , name "newTodo"
            , onInput UpdateField
            , onEnter Add
            ]
            []
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail "not ENTER"
    in
    on "keydown" (Json.andThen isEnter keyCode)



-- VIEW ALL ENTRIES


viewEntries : String -> List Entry -> Html Msg
viewEntries visibility entries =
    let
        isVisible todo =
            case visibility of
                "Completed" ->
                    get.completed todo

                "Active" ->
                    not (get.completed todo)

                _ ->
                    True

        allCompleted =
            List.all get.completed entries

        cssVisibility =
            if List.isEmpty entries then
                "hidden"

            else
                "visible"
    in
    section
        [ class "main"
        , style "visibility" cssVisibility
        ]
        [ input
            [ class "toggle-all"
            , type_ "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , Keyed.ul [ class "todo-list" ] <|
            List.map viewKeyedEntry (List.filter isVisible entries)
        ]



-- VIEW INDIVIDUAL ENTRIES


viewKeyedEntry : Entry -> ( String, Html Msg )
viewKeyedEntry todo =
    ( String.fromInt (get.id todo), lazy viewEntry todo )


viewEntry : Entry -> Html Msg
viewEntry todo =
    li
        [ classList [ ( "completed", get.completed todo ), ( "editing", get.editing todo ) ] ]
        [ div
            [ class "view" ]
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked (get.completed todo)
                , onClick (Check (get.id todo) (not (get.completed todo)))
                ]
                []
            , label
                [ onDoubleClick (EditingEntry (get.id todo) True) ]
                [ text (get.description todo) ]
            , button
                [ class "destroy"
                , onClick (Delete (get.id todo))
                ]
                []
            ]
        , input
            [ class "edit"
            , value (get.description todo)
            , name "title"
            , id ("todo-" ++ String.fromInt (get.id todo))
            , onInput (UpdateEntry (get.id todo))
            , onBlur (EditingEntry (get.id todo) False)
            , onEnter (EditingEntry (get.id todo) False)
            ]
            []
        ]



-- VIEW CONTROLS AND FOOTER


viewControls : String -> List Entry -> Html Msg
viewControls visibility entries =
    let
        entriesCompleted =
            List.length (List.filter get.completed entries)

        entriesLeft =
            List.length entries - entriesCompleted
    in
    footer
        [ class "footer"
        , hidden (List.isEmpty entries)
        ]
        [ lazy viewControlsCount entriesLeft
        , lazy viewControlsFilters visibility
        , lazy viewControlsClear entriesCompleted
        ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
    let
        item_ =
            if entriesLeft == 1 then
                " item"

            else
                " items"
    in
    span
        [ class "todo-count" ]
        [ strong [] [ text (String.fromInt entriesLeft) ]
        , text (item_ ++ " left")
        ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
    ul
        [ class "filters" ]
        [ visibilitySwap "#/" "All" visibility
        , text " "
        , visibilitySwap "#/active" "Active" visibility
        , text " "
        , visibilitySwap "#/completed" "Completed" visibility
        ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
    li
        [ onClick (ChangeVisibility visibility) ]
        [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
            [ text visibility ]
        ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
    button
        [ class "clear-completed"
        , hidden (entriesCompleted == 0)
        , onClick DeleteComplete
        ]
        [ text ("Clear completed (" ++ String.fromInt entriesCompleted ++ ")")
        ]


infoFooter : Html msg
infoFooter =
    footer [ class "info" ]
        [ p [] [ text "Double-click to edit a todo" ]
        , p []
            [ text "Modified by "
            , a [ href "https://github.com/s6o" ] [ text "Oliver Sõro" ]
            ]
        , p []
            [ text "Written by "
            , a [ href "https://github.com/evancz" ] [ text "Evan Czaplicki" ]
            ]
        , p []
            [ text "Part of "
            , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
            ]
        ]
