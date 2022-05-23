module Main exposing (..)

import Browser as Browser
import Html exposing (Html, b, div, h1, h3, h4, input, text, textarea)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { content : String
    , fontSize : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "Start typing..." 100
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateContent String
    | UpdateFontSize String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateContent content ->
            ( { model | content = content }
            , Cmd.none
            )

        UpdateFontSize fontSize ->
            ( { model | fontSize = Maybe.withDefault 100 (String.toInt fontSize) }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "padding" "2rem" ]
        [ h1 [] [ text "Bionic Reading Elm" ]
        , div []
            [ h4 []
                [ text "Font-Size:" ]
            , input
                [ onInput UpdateFontSize, type_ "range", Html.Attributes.min "100", Html.Attributes.max "500", value (String.fromInt model.fontSize) ]
                []
            , text (String.fromInt model.fontSize ++ "%")
            ]
        , div [ style "display" "flex", style "flex-direction" "row", style "gap" "1rem" ]
            [ div [ style "flex" "1" ]
                [ h3 [] [ text "Insert Text:" ]
                , textarea [ style "min-height" "30vh", onInput UpdateContent, placeholder model.content ] []
                ]
            , div [ style "flex" "1" ]
                [ h3 [] [ text "Read Text:" ]
                , div [ style "font-size" (String.fromInt model.fontSize ++ "%") ] [ contentView model.content ]
                ]
            ]
        ]


contentView : String -> Html msg
contentView content =
    String.split "\n" content |> List.map paragraphView |> div []


paragraphView : String -> Html msg
paragraphView paragraph =
    String.split " " paragraph |> List.concatMap formatWordAndAddWhiteSpace |> div []


formatWordAndAddWhiteSpace : String -> List (Html msg)
formatWordAndAddWhiteSpace word =
    let
        formatedWord =
            if wordStartWithNumber word then
                [ text word ]

            else
                formatWord word
    in
    List.append [ whiteSpace ] formatedWord


formatWord : String -> List (Html msg)
formatWord word =
    let
        wordLength =
            String.filter (\c -> c /= '.') word |> String.length
    in
    case wordLength of
        1 ->
            [ b [] [ text word ] ]

        2 ->
            [ String.left 1 word |> text |> List.singleton |> b [], String.right 1 word |> text ]

        3 ->
            [ String.left 1 word |> text |> List.singleton |> b [], String.right 2 word |> text ]

        4 ->
            [ String.left 2 word |> text |> List.singleton |> b [], String.right 2 word |> text ]

        _ ->
            let
                midle =
                    floor (toFloat wordLength * 3 / 5)
            in
            [ String.left midle word |> text |> List.singleton |> b [], String.right (String.length word - midle) word |> text ]


wordStartWithNumber : String -> Bool
wordStartWithNumber word =
    List.range 0 9 |> List.map String.fromInt |> List.any (\c -> String.startsWith c word)


whiteSpace : Html msg
whiteSpace =
    text " "
