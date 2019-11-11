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
    }


init : Model
init =
    { input = ""
    , playing = Nothing
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
    div []
        [ h1 []
            [ span [ class "Title--Spotify" ] [ text "Spoti" ]
            , span [ class "Title--App" ] [ text "Gether" ]
            ]
        , case model.playing of
            Nothing ->
                div [ class "Track-Container" ]
                    [ span [ class "Track-Label" ] [ text "No label" ]
                    , span [ class "Track-Artist" ] [ text "No Artist" ]
                    , span [ class "Track-ID" ] [ text "- - -" ]
                    ]

            Just value ->
                div [ class "Track-Container" ]
                    [ span [ class "Track-Label" ] [ text value.track ]
                    , span [ class "Track-Artist" ] [ text value.artist ]
                    , span [ class "Track-ID" ] [ text value.id ]
                    ]
        , button [ onClick (SetPlaying { artist = "Elton John", track = "Rocket Man", id = "UUID_UUID_UUID" }) ] [ text "+" ]
        ]
