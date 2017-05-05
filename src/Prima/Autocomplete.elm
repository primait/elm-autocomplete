module Prima.Autocomplete exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, name, placeholder, autocomplete)
import Html.Events exposing (onInput, onClick)


type State
    = State Value


type alias Value =
    String


type alias Name =
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
        , threshold : Int
        , name : Name
        , placeholder : Placeholder
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


config : Name -> Placeholder -> Maybe data -> (Msg data -> State -> msg) -> (data -> Value) -> Config msg data
config name placeholder selected msgMapper suggestionsMapper =
    Config
        { name = name
        , placeholder = placeholder
        , msgMapper = msgMapper
        , suggestionsMapper = suggestionsMapper
        , selected = selected
        , threshold = 2
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
view ((Config { selected, threshold }) as config) state suggestionsList =
    let
        hasSelection =
            Maybe.map (always True) selected |> Maybe.withDefault False
    in
        div
            [ class "autocomplete"
            ]
            [ search config state
            , suggestions config state suggestionsList
                |> renderUnless hasSelection
            ]


search : Config msg data -> State -> Html msg
search (Config { msgMapper, suggestionsMapper, selected, threshold, placeholder, name }) state =
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
                            msgMapper (getSearchMsg threshold value) (State value)
                        )
    in
        input
            ([ class "autocomplete__search form__autocomplete"
             , event
             , Attr.placeholder placeholder
             , Attr.name name
             , Attr.id name
             , Attr.autocomplete False
             ]
                ++ currentValue
            )
            []


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
