module Main exposing (..)

import Browser
import FeatherIcons
import Html exposing (Html, button, code, div, h1, input, span, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchInput = ""
      , playing = Nothing
      , searchOpen = False
      , searchResults = []
      }
    , getNowPlaying
    )


type Msg
    = GotPlaying (Result Http.Error SpotifySong)
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
            ( { model | searchOpen = True }, Cmd.none )

        ChangeSearchInput input ->
            ( { model | searchInput = input }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
            [ input [ class "SearchBar-Text", placeholder "Search tracks", onInput ChangeSearchInput ] []
            , FeatherIcons.search |> FeatherIcons.toHtml []
            ]
        , div [ class "AppContainer" ]
            [ h1
                []
                [ span [ class "Title--Spotify" ] [ text "Spoti" ]
                , span [ class "Title--App" ] [ text "Gether" ]
                ]
            , div [ class "Track-Container" ]
                [ span [ class "Track-Label" ] [ text track.track ]
                , span [ class "Track-Artist" ] [ text track.artist ]
                , span [ class "Track-ID" ] [ text track.id ]
                ]
            , case model.searchOpen of
                True ->
                    div [ class "Search-Container" ]
                        [ input [] []
                        ]

                False ->
                    span [] []
            ]
        ]



-- HTTP-requests


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


getSearchResults : Cmd Msg
getSearchResults =
    Http.get
        { url = searchSongsUrl
        , expect = Http.expectJson GotSearch searchDecoder
        }



-- Decoders


songDecoder : D.Decoder SpotifySong
songDecoder =
    D.map3 SpotifySong (D.field "artist" D.string) (D.field "track" D.string) (D.field "id" D.string)


searchDecoder : D.Decoder (List SpotifySong)
searchDecoder =
    D.list songDecoder
