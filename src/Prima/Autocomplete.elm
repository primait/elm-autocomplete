module Prima.Autocomplete
    exposing
        ( State
        , Config
        , Customizations
        , Msg(..)
        , initialState
        , setInputValue
        , config
        , customConfig
        , view
        , hasData
        , hasNoData
        , isPristine
        )

{-|
 Documentation will come eventually ^_^

@docs State
@docs Config
@docs Customizations
@docs Msg
@docs initialState
@docs setInputValue
@docs config
@docs customConfig
@docs view
@docs hasData
@docs hasNoData
@docs isPristine
-}

import Html exposing (..)
import Html.Attributes exposing (class, classList, placeholder, autocomplete, value)
import Html.Events exposing (onInput, onClick)


{-|
  Holds the state of the Autocomplete.
-}
type State
    = State { value : String, isPristine : Bool }


{-|
  Messages your app can listen to.
-}
type Msg data
    = OnSearch { value : String, thresholdReached : Bool }
    | OnSelect data


{-|
  The autocomplete state UI wise.
-}
type ViewState msg
    = Pristine
    | WithData (Html msg)
    | NoData


{-|
  Config type.
-}
type Config data msg
    = Config
        { toMsg : ( State, Msg data ) -> msg
        , customizations : Customizations data msg
        }


{-|
  Customize the autocomplete!
-}
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


{-|
  Utility helper
-}
hasNoData : ViewState msg -> Bool
hasNoData viewState =
    case viewState of
        NoData ->
            True

        _ ->
            False


{-|
  Utility helper
-}
hasData : ViewState msg -> Bool
hasData viewState =
    case viewState of
        WithData _ ->
            True

        _ ->
            False


{-|
  Utility helper
-}
isPristine : ViewState msg -> Bool
isPristine viewState =
    case viewState of
        Pristine ->
            True

        _ ->
            False


defaultContainer : Html msg -> ViewState msg -> Html msg -> Html msg
defaultContainer noResultView viewState searchInput =
    let
        classes =
            [ ( "autocomplete__container", True )
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
            [ class "autocomplete__input", autocomplete False ]
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
    li [ class "autocomplete__item", onClick callback ] [ toHtml data ]


defaultCostumization : Customizations data msg
defaultCostumization =
    { placeholder = ""
    , threshold = 2
    , container = defaultContainer defaultNoResult
    , input = defaultInput
    , listContainer = defaultListContainer
    , elementContainer = always (text "configure me!")
    }


{-|
  Setup config.
-}
config : (( State, Msg data ) -> msg) -> (data -> Html msg) -> Config data msg
config toMsg toText =
    Config
        { toMsg = toMsg
        , customizations = { defaultCostumization | elementContainer = defaultElementContainer toText }
        }


{-|
  Setup custom config.
-}
customConfig :
    { toMsg : ( State, Msg data ) -> msg
    , customizations : Customizations data msg
    }
    -> Config data msg
customConfig data =
    Config data


{-|
  Initial state.
-}
initialState : State
initialState =
    setState ""


{-|
  Update the autocomplete value from your app.
-}
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


{-|
  Autocomplete view.
-}
view : Config data msg -> State -> List data -> Html msg
view (Config { toMsg, customizations }) (State state) items =
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
