module Main exposing (..)

import Browser
import Debounce exposing (Debounce)
import FeatherIcons
import Html exposing (Attribute, Html, button, code, div, h1, input, li, span, text, ul)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onBlur, onClick, onFocus, onInput)
import Http
import Json.Decode as D
import Task



-- Models


type alias SpotifySong =
    { artist : String
    , track : String
    , id : String
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
    | DebounceSearch Debounce.Msg
    | GotSearch (Result Http.Error (List SpotifySong))
    | SetSearchActive
    | ChangeSearchInput String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    { artist = "No artist", track = "No song", id = "- - -" }

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
                , span [ class "Track-Artist" ] [ text track.artist ]
                , span [ class "Track-ID" ] [ text track.id ]
                ]
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
    li [ class "SearchResult" ]
        [ span [ class "SearchResult-Label" ] [ text song.track ]
        , span [ class "SearchResult-Label" ] [ text song.artist ]
        ]



-- HTTP-requests


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



-- Decoders


songDecoder : D.Decoder SpotifySong
songDecoder =
    D.map3 SpotifySong (D.field "artist" D.string) (D.field "track" D.string) (D.field "id" D.string)


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
