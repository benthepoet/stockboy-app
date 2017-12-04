module Main exposing (..)

-- IMPORTS

import Html exposing (Html, button, div, form, input, p, span, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Navigation
import Request
import Route
import Time

-- TYPES

type alias Model = 
    { email: String
    , password: String
    , pollInterval: Time.Time
    , positions: List Request.PositionResponse
    , route: Maybe Route.Route
    , token: Maybe String
    }
    
type Msg 
    = LoadPositions (Result Http.Error (List Request.PositionResponse))
    | LoadToken (Result Http.Error Request.AuthResponse)
    | Poll Time.Time
    | RouteChange (Maybe Route.Route)
    | SignOut
    | SubmitCredentials
    | TypeEmail String
    | TypePassword String
    
-- PROGRAM
    
init location =
    ( Model "" "" (10 * 1000) [] (Route.parse location) Nothing
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
        
        RouteChange route ->
            ( { model | route = route }
            , Cmd.none
            )
        
        SignOut ->
            ( { model | token = Nothing } 
            , Cmd.none
            )
        
        SubmitCredentials ->
            ( model
            , Http.send LoadToken (Request.authenticate model.email model.password) 
            )
        
        TypeEmail email ->
            ( { model | email = email }
            , Cmd.none
            )
            
        TypePassword password ->
            ( { model | password = password }
            , Cmd.none
            )

subscriptions model = 
    Time.every model.pollInterval Poll

viewMyPositions model =
    div [] [ text "My Positions" ]
    
viewNotFound model =
    div [] [ text "Not Found" ]
    
viewSignIn model =
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
        , div [] 
            (case model.token of
                Just token ->
                    [ p [] [ text token ] 
                    , button [ Events.onClick SignOut ] [ text "Sign Out"] 
                    ]
                Nothing ->
                    []
            )
        ]

view model =
    case model.route of
        Just (Route.Protected Route.MyPositions) ->
            viewMyPositions model
        
        Just (Route.Public Route.SignIn) ->
            viewSignIn model
            
        Nothing ->
            viewNotFound model

main 
    = Navigation.program (Route.parse >> RouteChange)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        } 