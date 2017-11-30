module Main exposing (..)

-- IMPORTS

import Html exposing (Html, div, form, input, p, text)
import Html.Attributes as Attributes
import Html.Events as Events


-- TYPES

type alias Model = 
    { email: String
    , password: String
    , token: Maybe String
    }
    
type Msg 
    = TypeEmail String
    | TypePassword String
    | SubmitCredentials
    
-- PROGRAM
    
init =
    ( Model "" "" Nothing
    , Cmd.none)

update msg model = 
    case msg of
        TypeEmail email ->
            ( { model | email = email }
            , Cmd.none
            )
        TypePassword password ->
            ( { model | password = password }
            , Cmd.none
            )
        SubmitCredentials ->
            (model, Cmd.none)

view model =
    div [] 
        [ form [Events.onSubmit SubmitCredentials] 
            [ input 
                [ Attributes.type_ "text"
                , Events.onInput TypeEmail
                ] []
            , input 
                [ Attributes.type_ "text"
                , Events.onInput TypePassword
                ] []
            , input
                [ Attributes.type_ "submit"
                , Attributes.value "Sign In"
                ] []
            ]
        , p [] 
            (case model.token of
                Just token ->
                    [ text token ]
                Nothing ->
                    []
            )
        ]

main 
    = Html.program 
        { init = init
        , update = update
        , subscriptions = (\_ -> Sub.none)
        , view = view
        } 