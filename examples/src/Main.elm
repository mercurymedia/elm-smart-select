module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, href, style, target)
import MultiSelectExample
import SingleSelectExample
import SingleSelectRemoteExample
import Url exposing (Url)
import Url.Parser as Parser



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { page : Page
    , navKey : Nav.Key
    }


type Page
    = SingleSelect SingleSelectExample.Model
    | SingleSelectRemote SingleSelectRemoteExample.Model
    | MultiSelect MultiSelectExample.Model
    | NotFound


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | SingleSelectMsg SingleSelectExample.Msg
    | SingleSelectRemoteMsg SingleSelectRemoteExample.Msg
    | MultiSelectMsg MultiSelectExample.Msg



-- MODEL


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        initialModel =
            { page = NotFound, navKey = navKey }
    in
    changePageTo url initialModel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound ->
            Sub.none

        SingleSelect selectModel ->
            Sub.map SingleSelectMsg (SingleSelectExample.subscriptions selectModel)

        SingleSelectRemote selectModel ->
            Sub.map SingleSelectRemoteMsg (SingleSelectRemoteExample.subscriptions selectModel)

        MultiSelect selectModel ->
            Sub.map MultiSelectMsg (MultiSelectExample.subscriptions selectModel)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changePageTo url model

        ( SingleSelectMsg subMsg, SingleSelect subModel ) ->
            let
                ( updatedSubModel, pageCmd ) =
                    SingleSelectExample.update subMsg subModel
            in
            ( { model | page = SingleSelect updatedSubModel }, Cmd.map SingleSelectMsg pageCmd )

        ( SingleSelectRemoteMsg subMsg, SingleSelectRemote subModel ) ->
            let
                ( updatedSubModel, pageCmd ) =
                    SingleSelectRemoteExample.update subMsg subModel
            in
            ( { model | page = SingleSelectRemote updatedSubModel }, Cmd.map SingleSelectRemoteMsg pageCmd )

        ( MultiSelectMsg subMsg, MultiSelect subModel ) ->
            let
                ( updatedSubModel, pageCmd ) =
                    MultiSelectExample.update subMsg subModel
            in
            ( { model | page = MultiSelect updatedSubModel }, Cmd.map MultiSelectMsg pageCmd )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "elm-smart-select"
    , body =
        [ viewSidebar
        , viewPage model.page
        ]
    }


viewSidebar : Html Msg
viewSidebar =
    div [ class "sidebar" ]
        [ viewHeader
        , viewSources
        , viewNavigation
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "header" ] [ text "elm-smart-select" ]


viewSources : Html Msg
viewSources =
    div [ style "margin-top" "2rem" ]
        [ a
            [ href "https://github.com/mercurymedia/elm-smart-select"
            , target "_blank"
            ]
            [ text "Github" ]
        , span
            [ style "padding" "0px 8px"
            ]
            [ text "|" ]
        , a
            [ href "https://package.elm-lang.org/packages/mercurymedia/elm-smart-select/latest/"
            , target "_blank"
            ]
            [ text "Docs" ]
        ]


viewNavigation : Html Msg
viewNavigation =
    div [ class "navigation" ]
        [ div [] [ text "Examples" ]
        , viewPageLink "SingleSelect" "/"
        , viewPageLink "MultiSelect" "/multi"
        ]


viewPageLink : String -> String -> Html Msg
viewPageLink title url =
    a
        [ href url
        , style "margin-left" "15px"
        ]
        [ text title ]


viewPage : Page -> Html Msg
viewPage page =
    let
        toPage toMsg pageView =
            Html.map toMsg pageView
    in
    div
        [ class "page"
        ]
        [ case page of
            NotFound ->
                text "Not found"

            SingleSelect pageModel ->
                toPage SingleSelectMsg (SingleSelectExample.view pageModel)

            SingleSelectRemote pageModel ->
                toPage SingleSelectRemoteMsg (SingleSelectRemoteExample.view pageModel)

            MultiSelect pageModel ->
                toPage MultiSelectMsg (MultiSelectExample.view pageModel)
        ]



-- HELPER


changePageTo : Url -> Model -> ( Model, Cmd Msg )
changePageTo url model =
    let
        toPage toModel toMsg ( pageModel, pageCmd ) =
            ( { model | page = toModel pageModel }, Cmd.map toMsg pageCmd )

        parser =
            Parser.oneOf
                [ Parser.map (SingleSelectExample.init |> toPage SingleSelect SingleSelectMsg) Parser.top
                , Parser.map (MultiSelectExample.init |> toPage MultiSelect MultiSelectMsg) (Parser.s "multi")
                ]
    in
    Parser.parse parser url
        |> Maybe.withDefault ( { model | page = NotFound }, Cmd.none )
