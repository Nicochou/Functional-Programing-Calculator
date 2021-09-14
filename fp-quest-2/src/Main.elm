module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (for, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import String exposing (fromInt)


type alias Model =
    { i : Int
    , input : Maybe Int
    , output : Maybe Int
    }


initialModel : Model
initialModel =
    { i = 0
    , input = Nothing
    , output = Nothing
    }


view : Model -> Html Msg
view model =
    Html.form [ onSubmit SubmitForm ]
        [ Html.label [ for "i" ] [ text "Iteration" ]
        , Html.input [ type_ "number", id "iteration", value <| String.fromInt model.i, onInput Iterations ] []
        , Html.label [ for "input" ] [ text "input" ]
        , Html.input [ type_ "number", id "input", value <| maybeIntToString model.input, onInput SetValue ] []
        , Html.button [ onClick SubmitForm, id "compute" ] [ text "compute" ]
        , Html.output [ id "output", value <| maybeIntToString model.output ]
            []
        ]


maybeIntToString : Maybe Int -> String
maybeIntToString t =
    case t of
        Nothing ->
            ""

        Just v ->
            fromInt v


type Msg
    = Iterations String
    | SetValue String
    | SubmitForm


update : Msg -> Model -> Model
update msg model =
    case msg of
        Iterations i ->
            { model | i = Maybe.withDefault 0 <| String.toInt i }

        SetValue v ->
            { model | input = String.toInt v }

        SubmitForm ->
            { model | output = computeNextValueWithIteration model.i <| Maybe.withDefault 0 model.input }


computeNextValueWithIteration : Int -> Int -> Maybe Int
computeNextValueWithIteration i input =
    case i of
        0 ->
            if(input ==0) then Nothing else Just input

        _ ->
            computeNextValueWithIteration (i - 1) (computeNextValue input)


computeNextValue : Int -> Int
computeNextValue input =
    String.fromInt input
        |> String.split ""
        |> List.filterMap String.toInt
        |> List.sum
        |> (+) input


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }