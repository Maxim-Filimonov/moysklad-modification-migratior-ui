module Auth exposing (Model, Msg, init, update, view)

import Base64
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (attribute, class, disabled, href, placeholder)
import Html.Styled.Events exposing (onClick, onInput)


type alias Model =
    { username : String
    , password : String
    }


buildAuthorizationToken : Model -> String
buildAuthorizationToken model =
    "Basic " ++ Base64.encode (model.username ++ ":" ++ model.password)


type Msg
    = ChangePassword String
    | ChangeLogin String
    | Submit


view : Model -> List (Html Msg)
view model =
    [ div [ class "flex w-full justify-center" ]
        [ form [ class "bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4" ]
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
                        , Html.Styled.Attributes.type_ "text"
                        , Html.Styled.Attributes.value model.password
                        , onInput ChangePassword
                        ]
                        []
                    ]
                ]
            ]
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLogin login ->
            ( { model | username = login }, Cmd.none )

        ChangePassword password ->
            ( model, Cmd.none )

        Submit ->
            ( model, Cmd.none )


init : Model
init =
    { username = "", password = "" }
