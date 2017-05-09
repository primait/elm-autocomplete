module Prima.Autocomplete
    exposing
        ( State
        , Config
        , Customizations
        , Msg(..)
        , initialState
        , setInputValue
        , config
        , view
        , hasNoData
        , isPristine
        )

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, placeholder, autocomplete, value)
import Html.Events exposing (onInput, onClick)


type State
    = State { value : String, isPristine : Bool }


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


hasNoData : ViewState msg -> Bool
hasNoData viewState =
    case viewState of
        NoData ->
            True

        _ ->
            False


hasData : ViewState msg -> Bool
hasData viewState =
    case viewState of
        WithData _ ->
            True

        _ ->
            False


isPristine : ViewState msg -> Bool
isPristine viewState =
    case viewState of
        Pristine ->
            True

        _ ->
            False



-- defaultConfig =


defaultContainer : Html msg -> ViewState msg -> Html msg -> Html msg
defaultContainer noResultView viewState searchInput =
    let
        classes =
            [ ( "autocomplete", True )
            , ( "no-result", hasNoData viewState )
            , ( "has-data", hasData viewState )
            ]

        content =
            case viewState of
                Pristine ->
                    text ""

                WithData items ->
                    items

                NoData ->
                    noResultView
    in
        div [ classList classes ]
            [ searchInput
            , content
            ]


defaultInput : List (Attribute msg) -> Html msg
defaultInput attrs =
    let
        attributes =
            [ class "autocomplete__input", Attr.autocomplete False ]
                |> List.append attrs
    in
        input attributes []


defaultListContainer : (( data, msg ) -> Html msg) -> List ( data, msg ) -> Html msg
defaultListContainer toHtml items =
    ul [] (List.map toHtml items)


defaultNoResult : Html msg
defaultNoResult =
    div [] [ text "No result found" ]


defaultElementContainer : (data -> Html msg) -> ( data, msg ) -> Html msg
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
    setState ""


setInputValue : String -> State -> State
setInputValue value (State state) =
    setState value


setState : String -> State
setState value =
    State { value = value, isPristine = True }


isAboveThreshold : Int -> String -> Bool
isAboveThreshold threshold value =
    threshold <= String.length value


getSearchMsg : Int -> String -> ( State, Msg data )
getSearchMsg threshold value =
    let
        thresholdReached =
            isAboveThreshold threshold value
    in
        ( State { value = value, isPristine = False }
        , OnSearch { value = value, thresholdReached = thresholdReached }
        )


view : Config data msg -> State -> List data -> Html msg
view (Config { toMsg, toText, customizations }) (State state) items =
    let
        threshold =
            customizations.threshold

        searchInput =
            customizations.input
                [ onInput <| \string -> toMsg (getSearchMsg threshold string)
                , value state.value
                ]

        resultList =
            List.map (\data -> ( data, toMsg <| ( setState state.value, OnSelect data ) )) items
                |> customizations.listContainer customizations.elementContainer

        viewState =
            if state.isPristine || (not <| isAboveThreshold threshold state.value) then
                Pristine
            else
                case List.length items of
                    0 ->
                        NoData

                    _ ->
                        WithData resultList
    in
        customizations.container viewState searchInput
