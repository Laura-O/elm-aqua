module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)
import Navigation



---- MODEL ----


type alias Model =
    { currentRoute : Navigation.Location
    , values : List Value
    }


type alias Value =
    { id : Int
    , name : String
    }


type RoutePath
    = DefaultRoute
    | HomeRoute
    | AboutRoute
    | ValuesRoute
    | ValueRoute String
    | NotFoundRoute


initialValues : List Value
initialValues =
    [ Value 1 "KH"
    , Value 2 "pH"
    , Value 3 "Fe"
    ]


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    { currentRoute = location
    , values = initialValues
    }
        ! []



---- UPDATE ----


type Msg
    = UrlChange Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            { model | currentRoute = location } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


fromUrlHash : String -> RoutePath
fromUrlHash urlHash =
    let
        hashList =
            urlHash |> String.split "/" |> drop 1
    in
    case hashList of
        [] ->
            DefaultRoute

        [ "home" ] ->
            HomeRoute

        [ "about" ] ->
            AboutRoute

        [ "values" ] ->
            ValuesRoute

        [ "value", id ] ->
            ValueRoute id

        _ ->
            NotFoundRoute



---- VIEW ----


valueFromId : List Value -> String -> Maybe Value
valueFromId values idStr =
    let
        id =
            Result.withDefault 0 (String.toInt idStr)
    in
    List.filter (\value -> id == value.id) values
        |> head


homePage : Html Msg
homePage =
    h1 [] [ text "Home" ]


aboutPage : Html Msg
aboutPage =
    h1 [] [ text "About" ]


notFoundPage : Html Msg
notFoundPage =
    h1 [] [ text "Page Not Found" ]


valuesPage : Model -> Html Msg
valuesPage model =
    div []
        [ h1 [] [ text "Values" ]
        , ul []
            (List.map
                (\value ->
                    li [] [ link value.name ("/#/value/" ++ toString value.id) ]
                )
                model.values
            )
        ]


valuePage : Model -> String -> Html Msg
valuePage model idStr =
    let
        value =
            valueFromId model.values idStr
    in
    case value of
        Just u ->
            div []
                [ h1 []
                    [ text "Value Profile"
                    , div [] [ text (toString u.name) ]
                    ]
                ]

        Nothing ->
            div []
                [ h1 [] [ text "Value not found" ]
                ]


pageBody : Model -> Html Msg
pageBody model =
    let
        routePath =
            fromUrlHash model.currentRoute.hash
    in
    case routePath of
        DefaultRoute ->
            homePage

        HomeRoute ->
            homePage

        AboutRoute ->
            aboutPage

        ValuesRoute ->
            valuesPage model

        ValueRoute id ->
            valuePage model id

        NotFoundRoute ->
            notFoundPage


link : String -> String -> Html Msg
link name url =
    a [ href url ] [ text name ]


pageHeader : Html Msg
pageHeader =
    header []
        [ section [ class "navbar-section" ]
            [ a [ class "btn btn-link" ] [ link "Home" "#/home" ]
            , a [ class "btn btn-link" ] [ link "About" "#/about" ]
            , a [ class "btn btn-link" ] [ link "Values" "#/values" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ pageHeader
        , pageBody model
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
