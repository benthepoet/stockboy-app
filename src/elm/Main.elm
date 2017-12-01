module Main exposing (..)

-- IMPORTS

import Html exposing (Html, div, form, input, p, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Request
import Time

-- TYPES

type alias Model = 
    { email: String
    , password: String
    , pollInterval: Time.Time
    , positions: List Request.PositionResponse
    , token: Maybe String
    }
    
type Msg 
    = LoadPositions (Result Http.Error (List Request.PositionResponse))
    | LoadToken (Result Http.Error Request.AuthResponse)
    | Poll Time.Time
    | TypeEmail String
    | TypePassword String
    | SubmitCredentials
    
-- PROGRAM
    
init =
    ( Model "" "" (10 * 1000) [] Nothing
    , Cmd.none)

update msg model = 
    case msg of
        LoadPositions (Ok positions) ->
            ( { model | positions = positions }
            , Cmd.none 
            )
        
        LoadPositions (Err _) ->
            ( model, Cmd.none )
        
        LoadToken (Ok authResponse) ->
            ( { model | token = Just authResponse.token }
            , Cmd.none
            )        
        
        LoadToken (Err _) ->
            ( model, Cmd.none )
            
        Poll time ->
            case model.token of
                Just token ->
                    ( model
                    , Http.send LoadPositions (Request.getPositions token)
                    )
                Nothing ->
                    ( model, Cmd.none )
        
        TypeEmail email ->
            ( { model | email = email }
            , Cmd.none
            )
        TypePassword password ->
            ( { model | password = password }
            , Cmd.none
            )
        SubmitCredentials ->
            ( model
            , Http.send LoadToken (Request.authenticate model.email model.password) 
            )

subscriptions model = 
    Time.every model.pollInterval Poll

view model =
    div [] 
        [ form [Events.onSubmit SubmitCredentials] 
            [ input 
                [ Attributes.type_ "text"
                , Events.onInput TypeEmail
                ] []
            , input 
                [ Attributes.type_ "password"
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
        , subscriptions = subscriptions
        , view = view
        } 