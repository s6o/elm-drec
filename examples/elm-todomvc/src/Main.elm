module Main exposing (Msg(..), infoFooter, init, main, onEnter, update, updateWithStorage, view, viewControls, viewControlsClear, viewControlsCount, viewControlsFilters, viewEntries, viewEntry, viewInput, viewKeyedEntry, visibilitySwap)

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
import DRec
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Http
import Json.Decode as Json
import Json.Encode
import Model exposing (Entry, Fields(..), Model, emptyModel, newEntry)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = \model -> { title = "Elm • TodoMVC • PostgREST", body = view model }
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
        , url = "http://localhost:3000/model"
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
        , url = "http://localhost:3000/model"
        , body = Http.jsonBody (DRec.encode model)
        , expect = Http.expectJson (\_ -> NoOp) (DRec.decoder model)
        , timeout = Nothing
        , tracker = Nothing
        }



-- MODEL
-- The full application state of our todo app.


{-| Helper to access DRec based `Entry` and `Model` members.
-}
get :
    { entries : Model -> List Entry
    , field : Model -> String
    , uid : Model -> Int
    , visibility : Model -> String
    , description : Entry -> String
    , completed : Entry -> Bool
    , editing : Entry -> Bool
    , id : Entry -> Int
    }
get =
    { entries = DRec.getWith Entries (DRec.toList DRec.toDRec) []
    , field = DRec.get Field >> DRec.toString >> Result.withDefault ""
    , uid = DRec.get Uid >> DRec.toInt >> Result.withDefault 0
    , visibility = DRec.get Visibility >> DRec.toString >> Result.withDefault "All"
    , description = DRec.get Description >> DRec.toString >> Result.withDefault ""
    , completed = DRec.get Completed >> DRec.toBool >> Result.withDefault True
    , editing = DRec.get Editing >> DRec.toBool >> Result.withDefault False
    , id = DRec.get Id >> DRec.toInt >> Result.withDefault 0
    }


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


view : Model -> List (Html Msg)
view drec =
    [ stylesheet
    , div
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


stylesheet : Html msg
stylesheet =
    let
        t =
            Html.Attributes.property "type" (Json.Encode.string "text/css")
    in
    Html.node "style" [ t ] [ text css ]


css : String
css =
    """html,
body {
    margin: 0;
    padding: 0;
}

.todomvc-wrapper {
    visibility: visible !important;
}

button {
    margin: 0;
    padding: 0;
    border: 0;
    background: none;
    font-size: 100%;
    vertical-align: baseline;
    font-family: inherit;
    font-weight: inherit;
    color: inherit;
    -webkit-appearance: none;
    appearance: none;
    -webkit-font-smoothing: antialiased;
    -moz-font-smoothing: antialiased;
    font-smoothing: antialiased;
}

body {
    font: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;
    line-height: 1.4em;
    background: #f5f5f5;
    color: #4d4d4d;
    min-width: 230px;
    max-width: 550px;
    margin: 0 auto;
    -webkit-font-smoothing: antialiased;
    -moz-font-smoothing: antialiased;
    font-smoothing: antialiased;
    font-weight: 300;
}

button,
input[type="checkbox"] {
    outline: none;
}

.hidden {
    display: none;
}

.todoapp {
    background: #fff;
    margin: 130px 0 40px 0;
    position: relative;
    box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.2),
                0 25px 50px 0 rgba(0, 0, 0, 0.1);
}

.todoapp input::-webkit-input-placeholder {
    font-style: italic;
    font-weight: 300;
    color: #e6e6e6;
}

.todoapp input::-moz-placeholder {
    font-style: italic;
    font-weight: 300;
    color: #e6e6e6;
}

.todoapp input::input-placeholder {
    font-style: italic;
    font-weight: 300;
    color: #e6e6e6;
}

.todoapp h1 {
    position: absolute;
    top: -155px;
    width: 100%;
    font-size: 100px;
    font-weight: 100;
    text-align: center;
    color: rgba(175, 47, 47, 0.15);
    -webkit-text-rendering: optimizeLegibility;
    -moz-text-rendering: optimizeLegibility;
    text-rendering: optimizeLegibility;
}

.new-todo,
.edit {
    position: relative;
    margin: 0;
    width: 100%;
    font-size: 24px;
    font-family: inherit;
    font-weight: inherit;
    line-height: 1.4em;
    border: 0;
    outline: none;
    color: inherit;
    padding: 6px;
    border: 1px solid #999;
    box-shadow: inset 0 -1px 5px 0 rgba(0, 0, 0, 0.2);
    box-sizing: border-box;
    -webkit-font-smoothing: antialiased;
    -moz-font-smoothing: antialiased;
    font-smoothing: antialiased;
}

.new-todo {
    padding: 16px 16px 16px 60px;
    border: none;
    background: rgba(0, 0, 0, 0.003);
    box-shadow: inset 0 -2px 1px rgba(0,0,0,0.03);
}

.main {
    position: relative;
    z-index: 2;
    border-top: 1px solid #e6e6e6;
}

label[for='toggle-all'] {
    display: none;
}

.toggle-all {
    position: absolute;
    top: -55px;
    left: -12px;
    width: 60px;
    height: 34px;
    text-align: center;
    border: none; /* Mobile Safari */
}

.toggle-all:before {
    content: '❯';
    font-size: 22px;
    color: #e6e6e6;
    padding: 10px 27px 10px 27px;
}

.toggle-all:checked:before {
    color: #737373;
}

.todo-list {
    margin: 0;
    padding: 0;
    list-style: none;
}

.todo-list li {
    position: relative;
    font-size: 24px;
    border-bottom: 1px solid #ededed;
}

.todo-list li:last-child {
    border-bottom: none;
}

.todo-list li.editing {
    border-bottom: none;
    padding: 0;
}

.todo-list li.editing .edit {
    display: block;
    width: 506px;
    padding: 13px 17px 12px 17px;
    margin: 0 0 0 43px;
}

.todo-list li.editing .view {
    display: none;
}

.todo-list li .toggle {
    text-align: center;
    width: 40px;
    /* auto, since non-WebKit browsers doesn't support input styling */
    height: auto;
    position: absolute;
    top: 0;
    bottom: 0;
    margin: auto 0;
    border: none; /* Mobile Safari */
    -webkit-appearance: none;
    appearance: none;
}

.todo-list li .toggle:after {
    content: url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" width="40" height="40" viewBox="-10 -18 100 135"><circle cx="50" cy="50" r="50" fill="none" stroke="#ededed" stroke-width="3"/></svg>');
}

.todo-list li .toggle:checked:after {
    content: url('data:image/svg+xml;utf8,<svg xmlns="http://www.w3.org/2000/svg" width="40" height="40" viewBox="-10 -18 100 135"><circle cx="50" cy="50" r="50" fill="none" stroke="#bddad5" stroke-width="3"/><path fill="#5dc2af" d="M72 25L42 71 27 56l-4 4 20 20 34-52z"/></svg>');
}

.todo-list li label {
    white-space: pre-line;
    word-break: break-all;
    padding: 15px 60px 15px 15px;
    margin-left: 45px;
    display: block;
    line-height: 1.2;
    transition: color 0.4s;
}

.todo-list li.completed label {
    color: #d9d9d9;
    text-decoration: line-through;
}

.todo-list li .destroy {
    display: none;
    position: absolute;
    top: 0;
    right: 10px;
    bottom: 0;
    width: 40px;
    height: 40px;
    margin: auto 0;
    font-size: 30px;
    color: #cc9a9a;
    margin-bottom: 11px;
    transition: color 0.2s ease-out;
}

.todo-list li .destroy:hover {
    color: #af5b5e;
}

.todo-list li .destroy:after {
    content: '×';
}

.todo-list li:hover .destroy {
    display: block;
}

.todo-list li .edit {
    display: none;
}

.todo-list li.editing:last-child {
    margin-bottom: -1px;
}

.footer {
    color: #777;
    padding: 10px 15px;
    height: 20px;
    text-align: center;
    border-top: 1px solid #e6e6e6;
}

.footer:before {
    content: '';
    position: absolute;
    right: 0;
    bottom: 0;
    left: 0;
    height: 50px;
    overflow: hidden;
    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.2),
                0 8px 0 -3px #f6f6f6,
                0 9px 1px -3px rgba(0, 0, 0, 0.2),
                0 16px 0 -6px #f6f6f6,
                0 17px 2px -6px rgba(0, 0, 0, 0.2);
}

.todo-count {
    float: left;
    text-align: left;
}

.todo-count strong {
    font-weight: 300;
}

.filters {
    margin: 0;
    padding: 0;
    list-style: none;
    position: absolute;
    right: 0;
    left: 0;
}

.filters li {
    display: inline;
}

.filters li a {
    color: inherit;
    margin: 3px;
    padding: 3px 7px;
    text-decoration: none;
    border: 1px solid transparent;
    border-radius: 3px;
}

.filters li a.selected,
.filters li a:hover {
    border-color: rgba(175, 47, 47, 0.1);
}

.filters li a.selected {
    border-color: rgba(175, 47, 47, 0.2);
}

.clear-completed,
html .clear-completed:active {
    float: right;
    position: relative;
    line-height: 20px;
    text-decoration: none;
    cursor: pointer;
    position: relative;
}

.clear-completed:hover {
    text-decoration: underline;
}

.info {
    margin: 65px auto 0;
    color: #bfbfbf;
    font-size: 10px;
    text-shadow: 0 1px 0 rgba(255, 255, 255, 0.5);
    text-align: center;
}

.info p {
    line-height: 1;
}

.info a {
    color: inherit;
    text-decoration: none;
    font-weight: 400;
}

.info a:hover {
    text-decoration: underline;
}

/*
    Hack to remove background from Mobile Safari.
    Can't use it globally since it destroys checkboxes in Firefox
*/
@media screen and (-webkit-min-device-pixel-ratio:0) {
    .toggle-all,
    .todo-list li .toggle {
        background: none;
    }

    .todo-list li .toggle {
        height: 40px;
    }

    .toggle-all {
        -webkit-transform: rotate(90deg);
        transform: rotate(90deg);
        -webkit-appearance: none;
        appearance: none;
    }
}

@media (max-width: 430px) {
    .footer {
        height: 50px;
    }

    .filters {
        bottom: 10px;
    }
}"""
