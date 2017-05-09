module Prima.Autocomplete exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, placeholder, autocomplete, value)
import Html.Events exposing (onInput, onClick)


type State
    = State String


type Msg data
    = OnSearch { value : String, thresholdReached : Bool }
    | OnSelect data


type Config data msg
    = Config
        { toMsg : ( State, Msg data ) -> msg
        , toText : data -> Html msg
        , customizations : Customizations data msg
        }


type alias Customizations data msg =
    { placeholder : String
    , threshold : Int
    , container : List (Html msg) -> Html msg
    , input : List (Attribute msg) -> Html msg
    , listContainer :
        Maybe (Html msg)
        -> (( data, msg ) -> Html msg)
        -> List ( data, msg )
        -> Html msg
    , elementContainer : ( data, msg ) -> Html msg
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


defaultListContainer maybeMessage toHtml items =
    let
        children =
            List.map toHtml items

        hasNoSuggestions =
            List.length items == 0

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


defaultElementContainer toHtml ( data, callback ) =
    li [ class "autocomplete__suggestion", onClick callback ] [ toHtml data ]


defaultCostumization toText =
    { placeholder = ""
    , threshold = 2
    , container = defaultContainer
    , input = defaultInput
    , noResult = defaultNoResult
    , listContainer = defaultListContainer
    , elementContainer = defaultElementContainer toText
    }


config : (( State, Msg data ) -> msg) -> (data -> Html msg) -> Config data msg
config toMsg toText =
    Config
        { toMsg = toMsg
        , toText = toText
        , customizations = defaultCostumization toText
        }


initialState : State
initialState =
    State ""


isAboveThreshold : Int -> String -> Bool
isAboveThreshold threshold value =
    threshold <= String.length value


getSearchMsg : Int -> String -> ( State, Msg data )
getSearchMsg threshold value =
    let
        thresholdReached =
            isAboveThreshold threshold value
    in
        ( State value, OnSearch { value = value, thresholdReached = thresholdReached } )


view : Config data msg -> State -> List data -> Html msg
view (Config { toMsg, toText, customizations }) (State currentValue) items =
    let
        searchInput =
            customizations.input
                [ onInput <| \string -> toMsg (getSearchMsg customizations.threshold string)
                , value currentValue
                ]

        resultList =
            List.map (\data -> ( data, toMsg <| ( State currentValue, OnSelect data ) )) items
                |> customizations.listContainer Nothing customizations.elementContainer
    in
        customizations.container
            [ searchInput
            , resultList
            ]
