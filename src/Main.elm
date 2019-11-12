module Main exposing (..)

import Browser
import Debounce exposing (Debounce)
import FeatherIcons
import Html exposing (Attribute, Html, button, code, div, h1, input, li, span, text, ul)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Task



-- Models


type alias SpotifySong =
    { artist : String
    , track : String
    , id : String
    , album : String
    }


type alias Model =
    { searchInput : String
    , playing : Maybe SpotifySong
    , searchOpen : Bool
    , searchResults : List SpotifySong
    , searchDebouncer : Debounce String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


type Msg
    = GotPlaying (Result Http.Error SpotifySong)
    | MadeCommand (Result Http.Error SpotifySong)
    | GotSearch (Result Http.Error (List SpotifySong))
    | SendCommand String
    | SelectTrack String (Maybe String)
    | DebounceSearch Debounce.Msg
    | ChangeSearchInput String
    | SetSearchActive
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTrack command uuid ->
            ( model, postControlChange command uuid )

        SendCommand command ->
            ( model, postControlChange command Nothing )

        MadeCommand result ->
            case result of
                Ok song ->
                    ( { model | playing = Just song }, Cmd.none )

                Err _ ->
                    ( { model | playing = Nothing }, Cmd.none )

        GotPlaying result ->
            case result of
                Ok song ->
                    ( { model | playing = Just song }, Cmd.none )

                Err _ ->
                    ( { model | playing = Nothing }, Cmd.none )

        GotSearch result ->
            case result of
                Ok searchResults ->
                    ( { model | searchResults = searchResults }, Cmd.none )

                Err _ ->
                    ( { model | searchResults = [] }, Cmd.none )

        SetSearchActive ->
            ( { model | searchOpen = not model.searchOpen }, Cmd.none )

        ChangeSearchInput input ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.push
                        (debounceConfig DebounceSearch)
                        input
                        model.searchDebouncer
            in
            ( { model | searchInput = input, searchDebouncer = newDebouncer }
            , cmd
            )

        DebounceSearch msg_ ->
            let
                ( newDebouncer, cmd ) =
                    Debounce.update
                        (debounceConfig DebounceSearch)
                        (Debounce.takeLast search)
                        msg_
                        model.searchDebouncer
            in
            ( { model | searchDebouncer = newDebouncer }, cmd )

        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    let
        track =
            case model.playing of
                Nothing ->
                    { artist = " - ", track = " - ", album = " - ", id = "- - -" }

                Just value ->
                    value
    in
    div [ class "SpotiGether-App" ]
        [ div [ class "SearchBar" ]
            [ input
                [ class "SearchBar-Text"
                , placeholder "Search tracks"
                , onInput ChangeSearchInput
                , onFocus SetSearchActive
                , onBlur SetSearchActive
                ]
                []
            , FeatherIcons.search |> FeatherIcons.toHtml []
            ]
        , div [ class "AppContainer" ]
            [ h1
                []
                [ span [ class "Title--Spotify" ] [ text "Spoti" ]
                , span [ class "Title--App" ] [ text "Gether" ]
                ]
            , div [ class "Track-Container" ]
                [ span [ class "Track-Legend" ] [ text "Now playing:" ]
                , span [ class "Track-Label" ] [ text track.track ]
                , span [ class "Track-Extra" ] [ text ("Artisti: " ++ track.artist) ]
                , span [ class "Track-Extra" ] [ text ("Albumi: " ++ track.album) ]
                , span [ class "Track-Extra" ] [ text track.id ]
                ]
            , controls
            , case model.searchOpen of
                True ->
                    div [ class "SearchContainer" ]
                        [ if List.length model.searchResults > 0 then
                            ul []
                                (List.map searchItem model.searchResults)

                          else
                            span [] [ text "No results found" ]
                        ]

                False ->
                    span [] []
            ]
        ]


searchItem : SpotifySong -> Html Msg
searchItem song =
    li [ class "SearchResult", onClick (SelectTrack "play" (Just song.id)) ]
        [ span [ class "SearchResult-Label" ] [ text song.track ]
        , span [ class "SearchResult-Label" ] [ text song.artist ]
        ]


controls : Html Msg
controls =
    div [ class "PlayerControls" ]
        [ button [ class "PlayerControls-Button", onClick (SendCommand "previous") ] [ FeatherIcons.skipBack |> FeatherIcons.toHtml [] ]
        , button [ class "PlayerControls-Button", onClick (SendCommand "play") ] [ FeatherIcons.play |> FeatherIcons.toHtml [] ]
        , button [ class "PlayerControls-Button", onClick (SendCommand "next") ] [ FeatherIcons.skipForward |> FeatherIcons.toHtml [] ]
        ]


debounceConfig : (Debounce.Msg -> Msg) -> Debounce.Config Msg
debounceConfig debounceMsg =
    { strategy = Debounce.later 1000, transform = debounceMsg }


search : String -> Cmd Msg
search input =
    if String.length input > 3 then
        getSearchResults input

    else
        Cmd.none


nowPlayingUrl : String
nowPlayingUrl =
    "http://localhost:5000/v1/spotify/now_playing"


searchSongsUrl : String
searchSongsUrl =
    "http://localhost:5000/v1/spotify/search"


postControlUrl : String
postControlUrl =
    "http://localhost:5000/v1/spotify/control"


getNowPlaying : Cmd Msg
getNowPlaying =
    Http.get
        { url = nowPlayingUrl
        , expect =
            Http.expectJson GotPlaying songDecoder
        }


getSearchResults : String -> Cmd Msg
getSearchResults input =
    Http.get
        { url = searchSongsUrl ++ "?query" ++ "=" ++ input
        , expect = Http.expectJson GotSearch searchDecoder
        }


postControlChange : String -> Maybe String -> Cmd Msg
postControlChange command uuid =
    let
        postBody =
            case uuid of
                Nothing ->
                    E.object [ ( "command", E.string command ) ]

                Just val ->
                    E.object
                        [ ( "command", E.string command )
                        , ( "uuid", E.string val )
                        ]
    in
    Http.request
        { url = postControlUrl
        , body =
            Http.jsonBody <| postBody
        , expect = Http.expectJson GotPlaying songDecoder
        , method = "POST"
        , timeout = Nothing
        , tracker = Nothing
        , headers = []
        }



-- Decoders


songDecoder : D.Decoder SpotifySong
songDecoder =
    D.map4 SpotifySong
        (D.field "artist" D.string)
        (D.field "track" D.string)
        (D.field "id" D.string)
        (D.field "album" D.string)


searchDecoder : D.Decoder (List SpotifySong)
searchDecoder =
    D.list songDecoder



-- Init


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchInput = ""
      , playing = Nothing
      , searchOpen = False
      , searchResults = []
      , searchDebouncer = Debounce.init
      }
    , getNowPlaying
    )


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }
