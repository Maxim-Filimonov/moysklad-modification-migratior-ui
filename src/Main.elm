module Main exposing (main)

import Auth exposing (Model, init, update, view)
import Browser
import Browser.Navigation as Nav
import Data
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href)
import Html.Styled.Events exposing (onClick)
import Http
import RemoteData exposing (RemoteData)
import Route exposing (Route(..))
import Url exposing (Url)
import View exposing (navIn, navOut)


type alias Flags =
    {}



-- MODEL


type User
    = User String


type Page
    = HomePage
    | LoginPage Auth.Model


type alias Model =
    { key : Nav.Key
    , route : Route.Route
    , package : RemoteData Http.Error Data.Package
    , user : Maybe User
    , currentPage : Page
    , auth : Auth.Model
    }


type Msg
    = -- Message naming conventions: https://youtu.be/w6OVDBqergc
      BrowserChangedUrl Url
    | UserClickedLink Browser.UrlRequest
    | UserClickedPackageButton
    | ServerRespondedWithPackage (Result Http.Error Data.Package)
    | GotAuthMessage Auth.Msg


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UserClickedLink
        , onUrlChange = BrowserChangedUrl
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


project : { description : String, title : String, url : String }
project =
    { title = "Elm Batteries Included!"
    , description = """
        Develop Elm apps with Parcel, Netlify, Tailwind CSS and Cypress.
        Learn how these delightful web technologies work together.
        Get started with Elm navigation, routes, remote data and JSON decoder
      """
    , url = "https://github.com/cedricss/elm-batteries#elm-batteries-included"
    }


view : Model -> Browser.Document Msg
view model =
    { title = project.title
    , body = [ body model |> div [] |> toUnstyled ]
    }


body : Model -> List (Html Msg)
body model =
    [ View.header
        [ navIn "Главная" "/"
        , navIn "Логин" "/login"
        ]
    , View.container <|
        case model.route of
            Home ->
                viewHome model

            NotFound ->
                View.notFound

            Login ->
                viewLogin model
    ]


viewLogin : Model -> List (Html Msg)
viewLogin model =
    Auth.view model.auth
        |> List.map (Html.Styled.map GotAuthMessage)


viewHome : Model -> List (Html Msg)
viewHome model =
    [ h1 [] [ text "Elm Batteries Included" ]
    , p
        [ class "max-w-2xl text-xl mb-4" ]
        [ text project.description ]
    , ul
        [ class "text-xl" ]
        [ li [] [ a [ href "/demo" ] [ text "Try the demo ›" ] ]
        , li [] [ a [ href project.url ] [ text "Read the documentation ›" ] ]
        ]
    ]


viewDemo : Model -> List (Html Msg)
viewDemo model =
    let
        content attributes =
            ul <|
                [ attribute "data-test" "package"
                , class "mt-8 flex flex-col justify-center max-w-2xl h-48"
                , class "bg-transition rounded"
                , class "pl-10 font-semibold leading-loose"
                ]
                    ++ attributes

        item key value =
            li [] (View.keyValue key value)

        fetchButton =
            button
                [ class "w-56"
                , attribute "data-action" "fetch-package"
                , onClick UserClickedPackageButton
                ]
                [ text "Fetch package.json" ]
    in
    [ h1 [] [ text "Demo" ]
    , h2 [] [ text "Serverless Lambda function on Netlify" ]
    , p
        [ class "max-w-xl text-xl mb-8" ]
        [ text "The demo function has a faux 500ms delay to simulate a slower connection and illustrate the loading state. "
        , a
            [ href "https://github.com/cedricss/elm-batteries/blob/master/functions/demo/demo.js#L5" ]
            [ text "Learn more ›" ]
        ]
    , div [] <|
        case model.package of
            RemoteData.Success p ->
                [ fetchButton
                , content
                    [ attribute "data-result" "success"
                    , class "bg-gray-300"
                    ]
                    [ item "name" p.name
                    , item "url" p.url
                    , item "author" p.author
                    , item "license" p.license
                    ]
                ]

            RemoteData.NotAsked ->
                [ fetchButton
                , content
                    [ attribute "data-result" "not-asked"
                    , class "bg-white"
                    ]
                    []
                ]

            RemoteData.Loading ->
                [ button
                    [ class "w-56 cursor-wait"
                    , disabled True
                    ]
                    [ text "Loading..." ]
                , content
                    [ attribute "data-result" "loading"
                    , class "bg-gray-500"
                    ]
                    []
                ]

            RemoteData.Failure _ ->
                [ fetchButton
                , content
                    [ attribute "data-result" "error"
                    , class "bg-red-500 p-12 text-white text-center"
                    ]
                    [ li [] [ text "Oops! Something went wrong..." ] ]
                ]
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserChangedUrl url ->
            ( { model | route = Route.fromUrl url }
            , Cmd.none
            )

        UserClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UserClickedPackageButton ->
            ( { model | package = RemoteData.Loading }
            , Http.get
                { url = "/.netlify/functions/demo"
                , expect =
                    Http.expectJson
                        ServerRespondedWithPackage
                        Data.packageDecoder
                }
            )

        ServerRespondedWithPackage result ->
            ( { model | package = RemoteData.fromResult result }
            , Cmd.none
            )

        GotAuthMessage authMessage ->
            let
                ( authModel, cmd ) =
                    Auth.update authMessage model.auth
            in
            ( { model | auth = authModel }, Cmd.map GotAuthMessage cmd )


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        route =
            Route.fromUrl url
    in
    ( { key = key
      , route = route
      , package = RemoteData.NotAsked
      , user = Nothing
      , auth = Auth.init
      , currentPage = HomePage
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none