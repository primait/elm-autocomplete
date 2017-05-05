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
        , customizations : Customizations msg data
        }


type alias Customizations msg data =
    { placeholder : Placeholder
    , threshold : Int
    , container : List (Html msg) -> Html msg
    , input : List (Attribute msg) -> Html msg
    , listContainer : Maybe (Html msg) -> (( Value, data, msg ) -> Html msg) -> List ( Value, data, msg ) -> Html msg
    , elementContainer : ( Value, data, msg ) -> Html msg
    , noResult : Html msg
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
    let
        attributes =
            [ class "autocomplete__input", Attr.autocomplete False ]
                |> List.append attrs
    in
        input attributes []


defaultListContainer maybeMessage suggestion suggestionsList =
    let
        children =
            List.map suggestion suggestionsList

        hasNoSuggestions =
            List.length suggestionsList == 0

        message =
            Maybe.withDefault (text "") maybeMessage
    in
        div
            [ classList [ ( "no-suggestions", hasNoSuggestions ) ] ]
            [ ul [] children
            , message
            ]


defaultNoResult =
    div [] [ text "No result found" ]


defaultSuggestion ( label, _, callback ) =
    li [ class "autocomplete__suggestion", onClick callback ] [ text label ]


defaultCostumization =
    { placeholder = ""
    , threshold = 2
    , container = defaultContainer
    , input = defaultInput
    , noResult = defaultNoResult
    , listContainer = defaultListContainer
    , elementContainer = defaultSuggestion
    }


config : (Msg data -> State -> msg) -> (data -> Value) -> Config msg data
config msgMapper suggestionsMapper =
    Config
        { msgMapper = msgMapper
        , suggestionsMapper = suggestionsMapper
        , customizations = defaultCostumization
        }


initialState : State
initialState =
    State ""


isAboveThreshold : Int -> Value -> Bool
isAboveThreshold threshold value =
    threshold <= String.length value


getSearchMsg : Int -> Value -> Msg data
getSearchMsg threshold value =
    if isAboveThreshold threshold value then
        OnSearch value
    else
        ThresholdNotReached


view : Config msg data -> State -> List data -> Maybe data -> Html msg
view ((Config { customizations }) as config) state suggestionsList selected =
    let
        hasSelection =
            Maybe.map (always True) selected |> Maybe.withDefault False
    in
        customizations.container
            [ search config state selected
            , suggestions config state suggestionsList
                |> renderUnless hasSelection
            ]


search : Config msg data -> State -> Maybe data -> Html msg
search (Config { msgMapper, suggestionsMapper, customizations }) ((State value) as state) selected =
    let
        currentValue =
            selected
                |> Maybe.map suggestionsMapper
                |> Maybe.withDefault value
                |> Attr.value

        event =
            case selected of
                Just _ ->
                    onClick (msgMapper OnUnselect (State ""))

                Nothing ->
                    onInput
                        (\newValue ->
                            msgMapper (getSearchMsg customizations.threshold newValue) (State newValue)
                        )
    in
        customizations.input [ event, currentValue, Attr.placeholder customizations.placeholder ]


itemForElement : Config msg data -> State -> data -> ( Value, data, msg )
itemForElement (Config { suggestionsMapper, msgMapper }) state item =
    ( suggestionsMapper item, item, msgMapper (OnSelect item) state )


suggestions : Config msg data -> State -> List data -> Html msg
suggestions ((Config { customizations }) as config) ((State value) as state) suggestionsList =
    let
        items =
            List.map (itemForElement config state) suggestionsList

        hasNoSuggestions =
            List.length suggestionsList == 0

        message =
            if (isAboveThreshold customizations.threshold value && hasNoSuggestions) then
                Just customizations.noResult
            else
                Nothing
    in
        customizations.listContainer message customizations.elementContainer items
