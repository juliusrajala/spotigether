module Main exposing (..)

import Browser
import Html exposing (Html, button, code, div, h1, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias SpotifySong =
    { artist : String
    , track : String
    , id : String
    }


type alias Model =
    { input : String
    , playing : Maybe SpotifySong
    , searchOpen : Bool
    }


init : Model
init =
    { input = ""
    , playing = Nothing
    , searchOpen = False
    }


type Msg
    = SetPlaying SpotifySong


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetPlaying song ->
            { model | playing = Just song }


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
    div []
        [ h1 []
            [ span [ class "Title--Spotify" ] [ text "Spoti" ]
            , span [ class "Title--App" ] [ text "Gether" ]
            ]
        , div [ class "Track-Container" ]
            [ span [ class "Track-Label" ] [ text track.track ]
            , span [ class "Track-Artist" ] [ text track.artist ]
            , span [ class "Track-ID" ] [ text track.id ]
            ]
        , button [ onClick (SetPlaying { artist = "Elton John", track = "Rocket Man", id = "UUID_UUID_UUID" }) ] [ text "+" ]
        ]
