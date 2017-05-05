module Prima.Autocomplete exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, placeholder, autocomplete)
import Html.Events exposing (onInput, onClick)


type State
    = State Value


type alias Value =
    String


type alias Placeholder =
    String


type Msg data
    = ThresholdNotReached
    | OnSearch Value
    | OnSelect data
    | OnUnselect


type Config msg data
    = Config
        { msgMapper : Msg data -> State -> msg
        , suggestionsMapper : data -> Value
        , selected : Maybe data
        , customizations : Customizations msg
        }


type alias Customizations msg =
    { placeholder : Placeholder
    , threshold : Int
    , container : List (Html msg) -> Html msg
    , input : List (Attribute msg) -> Html msg
    }


renderIf : Bool -> Html a -> Html a
renderIf check html =
    if check then
        html
    else
        text ""


renderUnless : Bool -> Html a -> Html a
renderUnless check =
    renderIf (not check)



-- defaultConfig =


defaultContainer content =
    div [ class "autocomplete" ] content


defaultInput attrs =
    input
        ([ class "autocomplete__input"
         , Attr.autocomplete False
         ]
            ++ attrs
        )
        []


config : Maybe data -> (Msg data -> State -> msg) -> (data -> Value) -> Config msg data
config selected msgMapper suggestionsMapper =
    Config
        { msgMapper = msgMapper
        , suggestionsMapper = suggestionsMapper
        , selected = selected
        , customizations =
            { placeholder = ""
            , threshold = 2
            , container = defaultContainer
            , input = defaultInput
            }
        }


initialState : State
initialState =
    State ""


getSearchMsg : Int -> Value -> Msg data
getSearchMsg threshold value =
    if threshold <= String.length value then
        OnSearch value
    else
        ThresholdNotReached


view : Config msg data -> State -> List data -> Html msg
view ((Config { selected, customizations }) as config) state suggestionsList =
    let
        hasSelection =
            Maybe.map (always True) selected |> Maybe.withDefault False
    in
        customizations.container
            [ search config state
            , suggestions config state suggestionsList
                |> renderUnless hasSelection
            ]


search : Config msg data -> State -> Html msg
search (Config { msgMapper, suggestionsMapper, selected, customizations }) state =
    let
        currentValue =
            case selected of
                Just suggestion ->
                    [ suggestionsMapper suggestion |> Attr.value ]

                Nothing ->
                    []

        event =
            case selected of
                Just _ ->
                    onClick (msgMapper OnUnselect (State ""))

                Nothing ->
                    onInput
                        (\value ->
                            msgMapper (getSearchMsg customizations.threshold value) (State value)
                        )
    in
        customizations.input
            ([ event, Attr.placeholder customizations.placeholder ]
                ++ currentValue
            )


suggestions : Config msg data -> State -> List data -> Html msg
suggestions config state suggestionsList =
    ul
        [ classList
            [ ( "autocomplete__suggestions", True )
            , ( "no-suggestions", List.length suggestionsList < 1 )
            ]
        ]
        (if List.length suggestionsList > 0 then
            List.map (suggestion config state) suggestionsList
         else
            [ noSuggestions ]
        )


suggestion : Config msg data -> State -> data -> Html msg
suggestion (Config { suggestionsMapper, msgMapper }) state item =
    li
        [ class "autocomplete__suggestion"
        , onClick (msgMapper (OnSelect item) state)
        ]
        [ item |> suggestionsMapper |> text ]


noSuggestions : Html msg
noSuggestions =
    li
        [ class "autocomplete__suggestion" ]
        [ "Nessun risultato trovato" |> text ]
