module Main exposing (..)

import Browser
import Browser.Events exposing (..)
import Char exposing (..)
import Dictionary exposing (..)
import Html exposing (div)
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode exposing (..)
import List exposing (..)
import Random exposing (..)
import String exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscribe
        }


type alias Model =
    { guesses : List Char
    , word : String
    }


type Msg
    = KeyPress String
    | Restart
    | Rnd Int


subscribe model =
    onKeyPress (Json.Decode.map KeyPress (Json.Decode.field "key" Json.Decode.string))


init =
    ( Model [] "", randomCmd )


update msg model =
    case msg of
        KeyPress k ->
            onKeyPressUpdate k model

        Restart ->
            init

        Rnd i ->
            onRandomUpdate i model


randomCmd =
    Random.generate Rnd (Random.int 1 (List.length dictionary))


onRandomUpdate value model =
    ( { model
        | word = Maybe.withDefault "" (head (drop (value - 1) dictionary))
      }
    , Cmd.none
    )


onKeyPressUpdate key model =
    let
        character =
            case String.toList key of
                [ c ] ->
                    Char.toLower c

                _ ->
                    '\u{0000}'

        validChoice =
            member character (choices model.guesses)

        playerLost =
            gameover model.word model.guesses
    in
    if validChoice && not playerLost then
        ( { model | guesses = model.guesses ++ [ character ] }, Cmd.none )

    else
        ( model, Cmd.none )


diff a b =
    -- a - b
    List.foldl (\e lst -> List.filter ((/=) e) lst) a b


choices guesses =
    diff (String.toList "abcdefghijklmnopqrstuvwxyz") guesses


badGuesses word guesses =
    List.length (diff guesses (String.toList (String.toLower word)))


gameover word guesses =
    let
        mistakes =
            badGuesses word guesses

        remainingLetters =
            diff (choices guesses) (diff (choices guesses) (String.toList (String.toLower word)))
    in
    (5 < mistakes) || (0 == List.length remainingLetters)



-- VIEW


viewGuesses lst =
    String.fromList (List.intersperse ' ' (List.map Char.toUpper lst))


viewWordClue : String -> List Char -> String
viewWordClue word guesses =
    let
        showCharacter c =
            if List.member (Char.toLower c) (choices guesses) then
                '_'

            else
                c
    in
    String.map showCharacter word


stretchWord word =
    replace "" " " word


view model =
    div
        [ Html.Attributes.style "font-family" "sans-serif"
        , Html.Attributes.style "text-align" "center"
        , Html.Attributes.style "margin" "2em"
        , Html.Attributes.style "font-size" "24pt"
        ]
        [ div
            [ Html.Attributes.style "margin" "inherit"
            ]
            [ Html.text "Hangman" ]
        , div
            []
            [ svg
                [ Svg.Attributes.width "180"
                , Svg.Attributes.height "300"
                , viewBox "0 0 18 30"
                , stroke "black"
                , fill "none"
                ]
                (List.concat (List.take (1 + badGuesses model.word model.guesses) image))
            , Html.button
                [ Html.Attributes.style "background-color" "mediumseagreen"
                , Html.Attributes.style "font-size" "24pt"
                , Html.Attributes.style "color" "white"
                , Html.Attributes.style "border-radius" "12px"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "position" "float"
                , Html.Events.onClick Restart
                ]
                [ Html.text "Restart" ]
            ]
        , div [ Html.Attributes.style "margin" "inherit" ]
            [ Html.text (stretchWord (viewWordClue model.word model.guesses))
            ]
        , div [ Html.Attributes.style "margin" "inherit" ] [ Html.text (viewGuesses model.guesses) ]
        ]


image =
    [ [ Svg.path [ d "M 12 3 v -3 h -10 v 30 h -2 h 15" ] [] ] -- gallows
    , [ Svg.circle [ cx "12", cy "7", r "3" ] [] ] -- head
    , [ Svg.path [ d "M 12 10 v 10" ] [] ] -- body
    , [ Svg.path [ d "M 12 13 l -5 -3" ] [] ] -- left arm
    , [ Svg.path [ d "M 12 13 l 5 -3" ] [] ] -- right arm
    , [ Svg.path [ d "M 12 20 l -4 6" ] [] ] -- left leg
    , [ Svg.path [ d "M 12 20 l 4 6" ] [] ] -- right leg
    ]
