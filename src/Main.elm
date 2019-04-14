module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode

-- Main

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


-- Hardcoded Data for the initial state

inputWords = hardThWords ++ " " ++ "thing thirsty thumb thick thin think thought thorough thrive"

hardThWords = "the then than they their them this that these those themselves therefore thee mother father brother leather"

-- Model

type TH = Soft | Hard

type alias Model =
    { words : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model inputWords, Cmd.none )


-- Update

type Msg
    = KeyDown
    | Correct
    | Incorrect



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| Decode.map KeyDown keyDecoder ]

keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


-- View

view : Model -> Html Msg
view model =
    div [ style "float" "right"
        , style "width" "50%"
        , style "margin-top" "50px"
        ]
        -- Css Triangle https://css-tricks.com/snippets/css/css-triangle/
        -- Left Triangle
        [ div [ style "width" "0"
              , style "height" "0"
              , style "border-top" "20px solid transparent"
              , style "border-bottom" "20px solid transparent"
              , style "border-right" "20px solid blue"
              , style "float" "left"
              ]
            []
        -- Right Triangle
        , div [ style "width" "0"
              , style "height" "0"
              , style "border-top" "20px solid transparent"
              , style "border-bottom" "20px solid transparent"
              , style "border-left" "20px solid green"
              , style "margin-left" "30px"
              ]
            []
        , div []
            [ model.words
                |> String.split " " 
                |> List.head
                |> returnValue
                |> text
            ]
        ]

-- Custom Functions HELPERS

returnValue : Maybe String -> String
returnValue a =
    case a of
        Nothing ->
            ""
        
        Just value ->
            value