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

type Correctness = Correct | Incorrect

type alias Model =
    { words : List String
    , answer : Maybe TH
    , correctness : Maybe Correctness
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (String.split " " inputWords) Nothing Nothing, Cmd.none )


-- Update

type Msg
    = KeyDown String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            case key of
                "ArrowLeft" ->
                    -- Left means soft TH, check if it is present in the hard TH list then the answer is incorrect
                    if List.member (returnString <| List.head model.words) (String.split " " hardThWords) then
                        ( { model | correctness = Just Incorrect }, Cmd.none )
                    else
                        ( { model | correctness = Just Correct }, Cmd.none )

                "ArrowRight" ->
                    if List.member (returnString <| List.head model.words) (String.split " " hardThWords) then
                        ( { model | correctness = Just Correct }, Cmd.none )
                    else
                        ( { model | correctness = Just Incorrect }, Cmd.none )

                " " ->
                    ( { model | words = tail model.words }, Cmd.none )
                
                _ ->
                    ( model, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| Decode.map KeyDown keyDecoder]

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
                |> List.head
                |> returnString
                |> text
            ]
        , div []
            [text (checker model.correctness)]
        ]

-- Custom Functions HELPERS

-- a helper for the view, that outputs if the answer was correct or not
checker : Maybe Correctness -> String
checker value =
    case value of
        Nothing ->
            ""
        Just correctnessValue ->
            case correctnessValue of
                Correct -> "correct"
                Incorrect -> "incorrect"

returnString : Maybe String -> String
returnString a =
    case a of
        Nothing ->
            ""
        
        Just value ->
            value

tail : List a -> List a
tail list =
    case list of
        [] -> []
        x::xs -> xs