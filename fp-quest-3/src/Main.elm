module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, input, span, text, output)
import Html.Attributes exposing (for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import String exposing (fromInt)
import Html.Events exposing (onInput, onSubmit)
import Tuple exposing (first)
import List exposing(length)

type alias Model =
    { input : String
    , output : String
    }


init : Model
init =
    { input = ""
    , output = ""
    }

view : Model -> Html Msg
view model =
    div []
         [ input [ type_ "string", id "input",placeholder "Calcul polish (e.g:+546)", value model.input, onInput Setvalue] []
        , br [] []
        , output [ id "output", value <| model.output ]
        []
        ]



type Msg
    = NoOp
    | Setvalue String

update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Setvalue v ->
            { model | input = v, output = getSumPolish <| List.reverse (String.split " " v)  }






getSumPolish : List String ->  String
getSumPolish inputs =
     helperSum (List.filter (\x -> x /= "") inputs) []


calc : (Int -> Int -> Int) -> List String -> String -> String -> List String ->  String
calc func remainingInputs first second stack =
    case ( String.toInt first, String.toInt second ) of
        ( Just f, Just s ) ->
            helperSum remainingInputs <| String.fromInt (func f s) :: stack

        _ ->
            "ERROR"


helperSum : List String -> List String -> String
helperSum inputs digitsStack =
    let
        _ = Debug.log "inputs" inputs
        _ = Debug.log "digitsStack" digitsStack
    in
    case ( inputs, digitsStack ) of
        ( [], topOfStack :: _ ) -> if String.toInt topOfStack == Nothing || length digitsStack > 1 then "ERROR" else topOfStack
        

        ( "+" :: rest, first :: second :: stack ) ->
            if String.toInt first == Nothing || String.toInt second == Nothing then "ERROR" else calc (+) rest first second stack

        ( "-" :: rest, first :: second :: stack ) ->
            if String.toInt first == Nothing || String.toInt second == Nothing then "ERROR" else calc (-) rest first second stack

        ( "*" :: rest, first :: second :: stack ) ->
            if String.toInt first == Nothing || String.toInt second == Nothing then "ERROR" else calc (*) rest first second stack

        ( "/" :: rest, first :: second :: stack ) ->
             if second == "0" then "0" else calc (//) rest first second stack

        ( number :: rest, stack ) ->
            helperSum rest <| number :: stack
            

        _ ->
            "ERROR"



main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }