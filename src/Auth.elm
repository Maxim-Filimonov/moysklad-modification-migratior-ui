module Auth exposing (Model, Msg, init, update, view)

import Base64
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href, placeholder)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import RemoteData exposing (RemoteData, WebData)


type alias Model =
    { username : String
    , password : String
    , loginResponse : WebData JwtToken
    }


buildAuthorizationHeader : Model -> String
buildAuthorizationHeader model =
    "Basic " ++ Base64.encode (model.username ++ ":" ++ model.password)


type alias JwtToken =
    String


type LoginResponse
    = WebData JwtToken


loginRequest : Model -> Cmd Msg
loginRequest model =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" <| buildAuthorizationHeader model ]
        , expect = Http.expectString (RemoteData.fromResult >> GotLoginResponse)
        , timeout = Nothing
        , tracker = Nothing
        , url = "https://reify-modification-migrator.builtwithdark.com/login"
        , body = Http.emptyBody
        }


type Msg
    = ChangePassword String
    | ChangeLogin String
    | Submit
    | GotLoginResponse (WebData JwtToken)


view : Model -> List (Html Msg)
view model =
    let
        requestText =
            case model.loginResponse of
                RemoteData.Loading ->
                    "Загружаеться"

                RemoteData.NotAsked ->
                    ""

                RemoteData.Success jwt ->
                    "Логин успешен:" ++ jwt

                RemoteData.Failure error ->
                    case error of
                        Http.BadStatus status ->
                            if status == 403 then
                                "Неправильный логин или пароль"

                            else
                                "Ошибка статуса " ++ String.fromInt status

                        _ ->
                            "Неизвестная ошибка"
    in
    [ div [ class "flex w-full justify-center" ]
        [ form
            [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4"
            , Html.Styled.Events.onSubmit Submit
            ]
            [ div [ class "mb-4" ]
                [ label []
                    [ span [ class "block text-gray-700 text-sm font-bold mb-2" ] [ text "Логин" ]
                    , input
                        [ class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                        , placeholder "user@masterpainter"
                        , onInput ChangeLogin
                        , Html.Styled.Attributes.type_ "text"
                        , Html.Styled.Attributes.value model.username
                        ]
                        []
                    ]
                ]
            , div [ class "mb-4" ]
                [ label []
                    [ span [ class "block text-gray-700 text-sm font-bold mb-2" ] [ text "Пароль" ]
                    , input
                        [ class "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                        , placeholder "пароль"
                        , Html.Styled.Attributes.type_ "password"
                        , Html.Styled.Attributes.value model.password
                        , onInput ChangePassword
                        ]
                        []
                    ]
                ]
            , input
                [ class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded focus:outline-none focus:shadow-outline"
                , Html.Styled.Attributes.type_ "submit"
                , Html.Styled.Attributes.value "Логин"
                ]
                []
            , div [ class "mt-4 text-red-500" ] [ text requestText ]
            ]
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLogin login ->
            ( { model | username = login }, Cmd.none )

        ChangePassword password ->
            ( { model | password = password }, Cmd.none )

        Submit ->
            ( model, loginRequest model )

        GotLoginResponse response ->
            ( { model | loginResponse = response }, Cmd.none )


init : Model
init =
    { username = "", password = "", loginResponse = RemoteData.NotAsked }
