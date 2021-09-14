module Main exposing (Model, Msg(..), computeNextValue, init, main, maybeIntToString, numberToList, update, view)

import Array exposing (Array)
import Basics
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import String exposing (fromInt)


type alias Model =
    { input : Maybe Int
    , output : Maybe Int
    }



-- We initialize the default value of our model


init : Model
init =
    { input = Nothing
    , output = Nothing
    }



-- We render a view given the model


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "number",id "input", placeholder "Enter a number", value (maybeIntToString model.input), onInput SetValue ] []
        , br [] []
        , button [ onClick SubmitForm, id "compute" ] [ text "Calculate" ]
        , div [] [ text "output : ", span [ id "output" ] [ text <| maybeIntToString model.output ] ]
        ]



-- Helper function


maybeIntToString : Maybe Int -> String
maybeIntToString t =
    case t of
        Nothing ->
            ""

        Just v ->
            fromInt v


type Msg
    = NoOp
    | SubmitForm
    | SetValue String


update : Msg -> Model -> Model
update msg model =
    -- With Debug.log we log into the browser console, see https://package.elm-lang.org/packages/elm-lang/core/3.0.0/Debug
    case Debug.log "msg" msg of
        NoOp ->
            model

        SetValue v ->
            { model | input = String.toInt v }

        SubmitForm ->
            { model | output = Maybe.map computeNextValue model.input }



-- To implement


computeNextValue : Int -> Int
computeNextValue x =
    List.sum (numberToList x) + x


numberToList : Int -> List Int
numberToList number =
    String.fromInt number
        -- convert our Int to a String
        |> String.split ""
        |> List.filterMap String.toInt


main : Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }