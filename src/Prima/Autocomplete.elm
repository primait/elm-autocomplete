module Prima.Autocomplete exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, placeholder, autocomplete, value)
import Html.Events exposing (onInput, onClick)


type State
    = State String


type Msg data
    = OnSearch { value : String, thresholdReached : Bool }
    | OnSelect data


type ViewState msg
    = Pristine
    | WithData (Html msg)
    | NoData


type Config data msg
    = Config
        { toMsg : ( State, Msg data ) -> msg
        , toText : data -> Html msg
        , customizations : Customizations data msg
        }


type alias Customizations data msg =
    { placeholder : String
    , threshold : Int
    , container :
        ViewState msg
        -> Html msg
        -> Html msg
    , input : List (Attribute msg) -> Html msg
    , listContainer :
        (( data, msg ) -> Html msg)
        -> List ( data, msg )
        -> Html msg
    , elementContainer : ( data, msg ) -> Html msg
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


defaultContainer noResultView viewState searchInput =
    div [ class "autocomplete" ]
        [ searchInput
        , case viewState of
            Pristine ->
                text ""

            WithData items ->
                items

            NoData ->
                noResultView
        ]


defaultInput attrs =
    let
        attributes =
            [ class "autocomplete__input", Attr.autocomplete False ]
                |> List.append attrs
    in
        input attributes []


defaultListContainer toHtml items =
    ul [] (List.map toHtml items)


defaultNoResult =
    div [] [ text "No result found" ]


defaultElementContainer toHtml ( data, callback ) =
    li [ class "autocomplete__suggestion", onClick callback ] [ toHtml data ]


defaultCostumization toText =
    { placeholder = ""
    , threshold = 2
    , container = defaultContainer defaultNoResult
    , input = defaultInput
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


setInputValue : String -> State -> State
setInputValue value state =
    State value


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
        threshold =
            customizations.threshold

        searchInput =
            customizations.input
                [ onInput <| \string -> toMsg (getSearchMsg threshold string)
                , value currentValue
                ]

        resultList =
            List.map (\data -> ( data, toMsg <| ( State currentValue, OnSelect data ) )) items
                |> customizations.listContainer customizations.elementContainer

        viewState =
            if currentValue == "" || (not <| isAboveThreshold threshold currentValue) then
                Pristine
            else
                case List.length items of
                    0 ->
                        NoData

                    _ ->
                        WithData resultList
    in
        customizations.container viewState searchInput
