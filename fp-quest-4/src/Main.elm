module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text, br)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (list, string)
import Html exposing (output)

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }



-- Types


type Operator
    = Minus
    | Plus
    | Mult
    | Div


type ValidTerm
    = Op Operator
    | Integer Int
    | Url String


type Msg
    = GotInput String
    | GotParsed (Result String (List ValidTerm))
    | GotJson String (Result Http.Error String)
    | GotOutput String


inputMsg : String -> Msg
inputMsg s =
    GotInput s


type alias Model =
    { input : String
    , parsed : Result String (List ValidTerm)
    , output : String
    }



-- Elm architecture


init : flags -> ( Model, Cmd msg )
init _ =
    ( { input = "", parsed = Err "Empty", output = "ERROR" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( m, l ) =
            updateAux msg model []
    in
    ( m, Cmd.batch l )


updateAux : Msg -> Model -> List (Cmd Msg) -> ( Model, List (Cmd Msg) )
updateAux msg model cmds =
    case msg of
        GotInput input ->
            let
                new_model =
                    { model | input = input }
            in
            updateAux (GotParsed (jsonParsing ("[\"" ++ new_model.input ++ "\"]"))) new_model cmds

        GotParsed parsed ->
            let
                new_model =
                    { model | parsed = parsed }
            in
            let
                output =
                    eval new_model.parsed

                cmds_ =
                    urlCmds new_model.parsed
            in
            updateAux (GotOutput output) new_model (cmds_ ++ cmds)

        GotJson url json ->
            let
                parsedSubTerm =
                    replaceUrl url json model.parsed
            in
            updateAux (GotParsed parsedSubTerm) model cmds

        GotOutput output ->
            ( { model | output = output }, cmds )


view : Model -> Html Msg
view model =
        div []
         [ input [ type_ "text" , id "input", value model.input, onInput inputMsg ] []
        , br [] []
        , output [ id "output" ] [ text model.output ]
        ]



viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg_ =
    input [ type_ t, id p, value v, onInput toMsg_ ] []


viewOutput : String -> String -> Html msg
viewOutput p v =
    output [ id p ] [ text v ]


subscriptions : Model -> Sub Msg
subscriptions m =
    Sub.none



-- Helpers


jsonParsing : String -> Result String (List ValidTerm)
jsonParsing input =
    case Json.Decode.decodeString (Json.Decode.list string) input of
        Ok v ->
            decodeReverse v []

        Err e ->
            Err ("Failed to parse" ++ input)


decodeReverse : List String -> List ValidTerm -> Result String (List ValidTerm)
decodeReverse input acc =
    case input of
        [] ->
            Ok acc

        x :: xs ->
            case toValidTerm x of
                Ok t ->
                    decodeReverse xs (t :: acc)

                Err e ->
                    Err e


toValidTerm : String -> Result String ValidTerm
toValidTerm s =
    case String.toInt s of
        Nothing ->
            case s of
                "+" ->
                    Ok (Op Plus)

                "-" ->
                    Ok (Op Minus)

                "/" ->
                    Ok (Op Div)

                "*" ->
                    Ok (Op Mult)

                _ ->
                    Ok (Url s)

        Just i ->
            Ok (Integer i)


eval : Result String (List ValidTerm) -> String
eval resultTerms =
    case resultTerms of
        Ok terms ->
            case evalAux (List.reverse terms) [] of
                Ok i ->
                    String.fromInt i

                Err e ->
                    "ERROR"

        Err e ->
            "ERROR"


evalAux : List ValidTerm -> List ValidTerm -> Result String Int
evalAux t acc =
    case t of
        [] ->
            case acc of
                [ Integer i ] ->
                    Ok i

                [ Op _ ] ->
                    Err "Operator in accumulator"

                _ ->
                    Err ("Too many elements in accumulator" ++ vstoString acc)

        x :: xs ->
            case x of
                Integer i ->
                    evalAux xs (x :: acc)

                Op o ->
                    case acc of
                        first :: second :: tail ->
                            case [ first, second ] of
                                [ Integer t1, Integer t2 ] ->
                                    evalAux xs (Integer (calc o t1 t2) :: tail)

                                _ ->
                                    Err "Weird operands"

                        _ ->
                            Err (vstoString acc)

                Url u ->
                    Err "Bad url or not yet arrived"


calc : Operator -> Int -> Int -> Int
calc op t1 t2 =
    case op of
        Plus ->
            t1 + t2

        Minus ->
            t1 - t2

        Mult ->
            t1 * t2

        Div ->
            t1 // t2


vtoString : ValidTerm -> String
vtoString t =
    case t of
        Op Plus ->
            "+"

        Op Minus ->
            "-"

        Op Div ->
            "/"

        Op Mult ->
            "*"

        Integer i ->
            String.fromInt i

        Url u ->
            u


vstoString : List ValidTerm -> String
vstoString l =
    case l of
        [] ->
            ""

        x :: xs ->
            vtoString x ++ vstoString xs


urlCmds : Result String (List ValidTerm) -> List (Cmd Msg)
urlCmds resultTerms =
    case resultTerms of
        Ok t ->
            urlCmdsAux t []

        Err _ ->
            []


urlCmdsAux : List ValidTerm -> List (Cmd Msg) -> List (Cmd Msg)
urlCmdsAux terms acc =
    case terms of
        [] ->
            acc

        x :: xs ->
            case x of
                Url u ->
                    urlCmdsAux xs (Http.get { url = u, expect = Http.expectString (GotJson u) } :: acc)

                _ ->
                    urlCmdsAux xs acc


replaceUrl : String -> Result Http.Error String -> Result String (List ValidTerm) -> Result String (List ValidTerm)
replaceUrl url json parsed =
    case parsed of
        Ok p ->
            case json of
                Ok j ->
                    case jsonParsing j of
                        Ok t ->
                            Ok (replaceUrlAux url t p [])

                        Err e ->
                            Err e

                Err e ->
                    Err "Bad http request"

        Err e ->
            Err e


replaceUrlAux : String -> List ValidTerm -> List ValidTerm -> List ValidTerm -> List ValidTerm
replaceUrlAux url termsFromUrl parsed acc =
    case parsed of
        [] ->
            List.reverse acc

        x :: xs ->
            case x of
                Url u ->
                    if u == url then
                        replaceUrlAux url termsFromUrl xs (termsFromUrl ++ acc)

                    else
                        replaceUrlAux url termsFromUrl xs (x :: acc)

                _ ->
                    replaceUrlAux url termsFromUrl xs (x :: acc)
