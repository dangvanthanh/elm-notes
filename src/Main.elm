module Main exposing (Model, Msg(..), Note, appTodo, appTodoEditor, appTodoList, appToolbar, formmatTimestamp, formmatTitle, getFirstVisibleNote, getSelectedNote, init, main, subscriptions, transformNotes, update, view)

import Array
import Browser
import Html exposing (Html, button, div, input, p, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- init


type alias Note =
    { id : Int
    , body : String
    , timestamp : Int
    }


type alias Model =
    { notes : List Note, selectedNoteId : Int, searchNoteText : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes = []
      , selectedNoteId = 1
      , searchNoteText = ""
      }
    , Task.perform InitializeNotesTimestamps Time.now
    )



-- update


type Msg
    = InitializeNotesTimestamps Time.Posix
    | SelectNote Int
    | UpdateSelectedNoteBody String
    | UpdateSelectedNoteTimestamp Time.Posix
    | ClickNew
    | CreateNote Time.Posix
    | ClickDelete
    | InputSearch String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeNotesTimestamps time ->
            ( { model | notes = List.map (\note -> { note | timestamp = Time.posixToMillis time }) model.notes }, Cmd.none )

        SelectNote id ->
            ( { model | selectedNoteId = id }, Cmd.none )

        UpdateSelectedNoteBody newText ->
            case getSelectedNote model of
                Nothing ->
                    ( model, Cmd.none )

                Just selectedNote ->
                    let
                        updateSelectedNote note =
                            if note.id == model.selectedNoteId then
                                { note | body = newText }

                            else
                                note

                        newNotes =
                            List.map updateSelectedNote model.notes
                    in
                    ( { model | notes = newNotes }, Task.perform UpdateSelectedNoteTimestamp Time.now )

        UpdateSelectedNoteTimestamp newTime ->
            case getSelectedNote model of
                Nothing ->
                    ( model, Cmd.none )

                Just selectedNote ->
                    let
                        updateSelectedNote note =
                            if note.id == model.selectedNoteId then
                                { note | timestamp = Time.posixToMillis newTime }

                            else
                                note

                        newNotes =
                            List.map updateSelectedNote model.notes
                    in
                    ( { model | notes = newNotes }, Cmd.none )

        ClickNew ->
            ( model, Task.perform CreateNote Time.now )

        CreateNote newTime ->
            let
                newTimestamp =
                    Time.posixToMillis newTime

                newId =
                    newTimestamp
            in
            ( { model
                | notes = [ { id = newId, body = "", timestamp = newTimestamp } ] ++ model.notes
                , selectedNoteId = newId
              }
            , Cmd.none
            )

        ClickDelete ->
            let
                newNotes =
                    List.filter (\note -> note.id /= model.selectedNoteId) model.notes

                firstVisibleNote =
                    getFirstVisibleNote newNotes model.searchNoteText
            in
            case firstVisibleNote of
                Nothing ->
                    ( { model | notes = newNotes }, Cmd.none )

                Just availableNote ->
                    ( { model | notes = newNotes, selectedNoteId = availableNote.id }, Cmd.none )

        InputSearch searchNoteText ->
            let
                firstVisibleNote =
                    getFirstVisibleNote model.notes searchNoteText
            in
            case firstVisibleNote of
                Nothing ->
                    ( { model | searchNoteText = searchNoteText, selectedNoteId = -1 }, Cmd.none )

                Just availableNote ->
                    ( { model | searchNoteText = searchNoteText, selectedNoteId = availableNote.id }, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--view


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ appToolbar
        , div [ class "note-container" ]
            [ appTodoList model
            , appTodoEditor model
            ]
        ]


appToolbar : Html Msg
appToolbar =
    div [ class "toolbar" ]
        [ button [ class "toolbar-button", onClick ClickNew ] [ text "New" ]
        , button [ class "toolbar-button", onClick ClickDelete ] [ text "Delete" ]
        , input [ class "toolbar-search", type_ "text", placeholder "Search...", onInput InputSearch ] []
        ]


appTodoList : Model -> Html Msg
appTodoList model =
    div [ class "note-selectors" ]
        (model.notes
            |> transformNotes model.searchNoteText
            |> List.map (\note -> appTodo note model.selectedNoteId)
        )


appTodo : Note -> Int -> Html Msg
appTodo note selectedNoteId =
    div [ classList [ ( "note-selector", True ), ( "active", note.id == selectedNoteId ) ], onClick (SelectNote note.id) ]
        [ p [ class "note-selector-title" ] [ text (formmatTitle note.body) ]
        , p [ class "note-selector-timestamp" ] [ text (formmatTimestamp note.timestamp) ]
        ]


appTodoEditor : Model -> Html Msg
appTodoEditor model =
    case getSelectedNote model of
        Nothing ->
            div [ class "note-editor" ] []

        Just selectedNote ->
            div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ text (formmatTimestamp selectedNote.timestamp) ]
                , textarea [ class "note-editor-input", onInput UpdateSelectedNoteBody, value selectedNote.body ] []
                ]



-- helpers


getFirstVisibleNote : List Note -> String -> Maybe Note
getFirstVisibleNote notes searchText =
    notes
        |> transformNotes searchText
        |> List.head


transformNotes : String -> List Note -> List Note
transformNotes searchNoteText notes =
    notes
        |> List.filter (\note -> String.contains (String.toLower searchNoteText) (String.toLower note.body))
        |> List.sortBy .timestamp
        |> List.reverse


getSelectedNote : Model -> Maybe Note
getSelectedNote model =
    model.notes
        |> transformNotes model.searchNoteText
        |> List.filter (\note -> note.id == model.selectedNoteId)
        |> List.head


formmatTitle : String -> String
formmatTitle body =
    let
        maxLength =
            20

        length =
            String.length body
    in
    if length > maxLength then
        String.left (maxLength - 3) body ++ "..."

    else if length == 0 then
        "New Note"

    else
        body


formmatTimestamp : Int -> String
formmatTimestamp timestamp =
    let
        time =
            Time.millisToPosix timestamp

        hour =
            String.fromInt (Time.toHour Time.utc time)

        minute =
            String.fromInt (Time.toMinute Time.utc time)

        second =
            String.fromInt (Time.toSecond Time.utc time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second
