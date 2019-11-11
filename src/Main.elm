module Main exposing (..)

import Browser
import FeatherIcons
import Html exposing (Html, button, code, div, h1, input, span, text)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onClick, onInput)


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


type HttpRequest
    = Failure
    | Loading
    | Success (List SpotifySong)


type alias Model =
    { searchInput : String
    , playing : Maybe SpotifySong
    , searchOpen : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchInput = ""
      , playing = Nothing
      , searchOpen = False
      }
    , Cmd.none
    )


type Msg
    = SetPlaying SpotifySong
    | SetSearchActive
    | ChangeSearchInput String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlaying song ->
            ( { model | playing = Just song }, Cmd.none )

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
            , button [ onClick (SetPlaying { artist = "Elton John", track = "Rocket Man", id = "UUID_UUID_UUID" }) ] [ text "+" ]
            , case model.searchOpen of
                True ->
                    div [ class "Search-Container" ]
                        [ input [] []
                        ]

                False ->
                    span [] []
            ]
        ]
