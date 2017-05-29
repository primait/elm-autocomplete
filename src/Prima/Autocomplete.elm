module Prima.Autocomplete
    exposing
        ( State
        , Config
        , Customizations
        , Msg(..)
        , initialState
        , setInputValue
        , getInputValue
        , onKeyDown
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
@docs getInputValue
@docs onKeyDown
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
    = State (Maybe StateData)


type alias StateData =
    { value : String, selectedIndex : Maybe Int, isPristine : Bool }


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
        (( Bool, data, msg ) -> Html msg)
        -> List ( Bool, data, msg )
        -> Html msg
    , elementContainer : ( Bool, data, msg ) -> Html msg
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


defaultListContainer : (( Bool, data, msg ) -> Html msg) -> List ( Bool, data, msg ) -> Html msg
defaultListContainer toHtml items =
    ul [] (List.map toHtml items)


defaultNoResult : Html msg
defaultNoResult =
    div [] [ text "No result found" ]


defaultElementContainer : (data -> Html msg) -> ( Bool, data, msg ) -> Html msg
defaultElementContainer toHtml ( isSelected, data, callback ) =
    let
        classes =
            [ ( "autocomplete__item", True )
            , ( "autocomplete__item-active", isSelected )
            ]
    in
        li
            [ classList classes, onClick callback ]
            [ toHtml data ]


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
    setState "" True


{-|
  Update the autocomplete value from your app.
-}
setInputValue : String -> State -> State
setInputValue value (State state) =
    setState value True


{-|
  Get the current input value.
-}
getInputValue : State -> String
getInputValue (State state) =
    Maybe.map .value state
        |> Maybe.withDefault ""


increaseIndex : Int -> Int -> Int -> Int
increaseIndex margin total selected =
    let
        target =
            selected + margin
    in
        if (target < total && target >= 0) then
            target
        else
            selected


updateSelectedIndex : StateData -> Int -> Int -> State
updateSelectedIndex data margin total =
    let
        defaultValue =
            if margin == 1 then
                0
            else
                total - 1
    in
        State <|
            Just
                { data
                    | selectedIndex =
                        Maybe.map (increaseIndex margin total) data.selectedIndex
                            |> Maybe.withDefault defaultValue
                            |> Just
                }


selectCurrentIndex selectedIndex items =
    let
        helper index =
            List.indexedMap (,) items
                |> List.filter (\( i, _ ) -> i == index)
                |> List.head
                |> Maybe.map (\( _, item ) -> OnSelect item)
    in
        Maybe.andThen helper selectedIndex


{-|
  Handle Keyboard events.
-}
onKeyDown : Int -> List data -> State -> ( State, Maybe (Msg data) )
onKeyDown keyCode items (State state) =
    let
        total =
            List.length items
    in
        case state of
            Nothing ->
                ( State state, Nothing )

            Just data ->
                case keyCode of
                    40 ->
                        -- up
                        ( updateSelectedIndex data 1 total, Nothing )

                    38 ->
                        -- down
                        ( updateSelectedIndex data -1 total, Nothing )

                    13 ->
                        -- enter
                        ( State state, selectCurrentIndex data.selectedIndex items )

                    _ ->
                        ( State state, Nothing )


setState : String -> Bool -> State
setState value isPristine =
    State (Just { value = value, selectedIndex = Nothing, isPristine = isPristine })


isAboveThreshold : Int -> String -> Bool
isAboveThreshold threshold value =
    threshold <= String.length value


getSearchMsg : Int -> String -> ( State, Msg data )
getSearchMsg threshold value =
    let
        thresholdReached =
            isAboveThreshold threshold value
    in
        ( setState value False
        , OnSearch { value = value, thresholdReached = thresholdReached }
        )


createResultList customizations toMsg items inputValue selectedIndex =
    let
        selected =
            Maybe.map identity selectedIndex
                |> Maybe.withDefault -1

        mapper index data =
            ( index == selected, data, toMsg <| ( setState inputValue True, OnSelect data ) )
    in
        List.indexedMap mapper items
            |> customizations.listContainer customizations.elementContainer


{-|
  Autocomplete view.
-}
view : Config data msg -> State -> List data -> Html msg
view (Config { toMsg, customizations }) (State state) items =
    let
        threshold =
            customizations.threshold

        inputValue =
            getInputValue (State state)

        searchInput =
            customizations.input
                [ onInput <| \string -> toMsg (getSearchMsg threshold string)
                , value inputValue
                ]

        resultList =
            createResultList customizations toMsg items inputValue

        viewStateHelp data =
            if data.isPristine || not (isAboveThreshold threshold data.value) then
                Pristine
            else
                case List.length items of
                    0 ->
                        NoData

                    _ ->
                        WithData (resultList data.selectedIndex)

        viewState =
            Maybe.map viewStateHelp state
                |> Maybe.withDefault Pristine
    in
        customizations.container viewState searchInput
