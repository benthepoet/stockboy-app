module Main exposing (..)

-- IMPORTS

import Html exposing (Html, article, button, div, form, h1, h4, header, i, input, nav, p, section, span, text)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Navigation
import Request
import Route
import Task
import Time
import Util

-- TYPES

type alias Model = 
    { balance: Float
    , email: String
    , equity: Float
    , password: String
    , pollInterval: Time.Time
    , positions: List Request.PositionResponse
    , route: Route.Route
    , stock: Maybe Request.Stock
    , token: Maybe String
    }
    
type Msg 
    = LoadPositions (Result Http.Error (List Request.PositionResponse))
    | LoadStock (Result Http.Error Request.Stock)
    | LoadToken (Result Http.Error Request.AuthResponse)
    | LoadUser (Result Http.Error Request.User)
    | Poll Time.Time
    | RouteChange Route.Route
    | SignOut
    | SubmitCredentials
    | TypeEmail String
    | TypePassword String
    
-- PROGRAM
    
init location =
    let
        pollInterval = 10 * 1000
        route = Route.parse location
    in
        ( Model 0 "" 0 "" pollInterval [] route Nothing Nothing
        , Task.perform RouteChange (Task.succeed route))

calculateEquity positions =
    positions
        |> List.map (\n -> (toFloat n.totalUnits) * n.stock.lastPrice)
        |> List.sum

update msg model = 
    case msg of
        LoadPositions (Ok positions) ->
            ( { model 
                | equity = calculateEquity positions
                , positions = positions }
            , Cmd.none 
            )
        
        LoadPositions (Err _) ->
            ( model, Cmd.none )
        
        LoadStock (Ok stock) ->
            ( { model | stock = Just stock }
            , Cmd.none
            )
            
        LoadStock (Err _) ->
            ( model, Cmd.none )
        
        LoadToken (Ok authResponse) ->
            ( { model | token = Just authResponse.token }
            , Cmd.batch 
                [ Navigation.newUrl (Route.toPath <| Route.Protected Route.MyPositions)
                , Task.perform Poll Time.now
                ]
            )        
        
        LoadToken (Err _) ->
            ( model, Cmd.none )
            
        LoadUser (Ok user) ->
            ( { model | balance = user.balance }
            , Cmd.none
            )
        
        LoadUser (Err _) ->
            ( model, Cmd.none )
            
        Poll time ->
            case model.token of
                Just token ->
                    ( model
                    , Cmd.batch 
                        [ Http.send LoadPositions (Request.getPositions token)
                        , Http.send LoadUser (Request.getUser token)
                        ]
                    )
                Nothing ->
                    ( model, Cmd.none )
        
        RouteChange route ->
            case route of
                Route.Protected page ->
                    case model.token of 
                        Nothing ->
                            ( model, Navigation.modifyUrl (Route.toPath <| Route.Public Route.SignIn) ) 
                        Just token -> 
                            case page of
                                Route.StockPosition id ->
                                    ( { model | route = route }
                                    , Http.send LoadStock (Request.getStock token id)
                                    )
                                    
                                _ -> 
                                    ( { model | route = route }
                                    , Cmd.none
                                    )
                Route.Public page ->
                    ( { model | route = route }
                    , Cmd.none
                    )
        
        SignOut ->
            ( { model | token = Nothing } 
            , Navigation.newUrl (Route.toPath <| Route.Public Route.SignIn)
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
    div [ Attributes.class "row" ] 
        [ button 
            [ Attributes.type_ "button"
            , Events.onClick SignOut ] [ text "Sign Out" ]
        , article [ Attributes.class "card shadow my-account" ]
            [ header [] [ text "My Account" ]
            , section [ Attributes.class "padding" ]
                [ p [] 
                    [ text "Equity Balance" 
                    , span [ Attributes.class "float" ] [ text <| "$" ++ (Util.toFixed 2 model.equity) ]
                    ]
                , p [] 
                    [ text "Cash Balance" 
                    , span [ Attributes.class "float" ] [ text <| "$" ++ (Util.toFixed 2 model.balance) ]
                    ]
                , p [] 
                    [ text "Account Value" 
                    , span [ Attributes.class "float" ] [ text <| "$" ++ (Util.toFixed 2 (model.equity + model.balance)) ]
                    ]
                ]
            ]
        , div [] <| List.map viewPosition model.positions
        ]
    
viewNav model =
    nav [] []

viewNotFound model =
    div [] [ text "Not Found" ]
    
viewPosition position =
    article [ Attributes.class "card shadow position" ]
        [ section [ Attributes.class "padding" ]
            [ h1 []
                [ span 
                    [ Attributes.class "label success" ] 
                    [ text <| (Util.toFixed 2 (position.profitRatio * 100)) ++ "%" ]  
                ]
            , h4 [] [ text position.stock.symbol ]
            , p [] [ text <| (toString position.totalUnits) ++ " shares" ]
            ]
        ]
    
viewSignIn model =
    div [] 
        [ form [Events.onSubmit SubmitCredentials] 
            [ input 
                [ Attributes.class "stack"
                , Attributes.placeholder "Email"
                , Attributes.type_ "text"
                , Events.onInput TypeEmail
                ] []
            , input 
                [ Attributes.class "stack"
                , Attributes.placeholder "Password"
                , Attributes.type_ "password"
                , Events.onInput TypePassword
                ] []
            , button
                [ Attributes.class "stack"
                , Attributes.type_ "submit"
                ] 
                [ i [ Attributes.class "fa fa-sign-in" ] [] 
                , text "Sign In" 
                ]
            ]
        , div [] 
            [ p [] [ text "Not a member?" ]
            , input 
                [ Attributes.class "button" 
                , Attributes.type_ "button"
                , Attributes.value "Create Account"
                ] []
            ]
        ]

viewStockPosition model =
    div [] 
        ( case model.stock of
            Nothing -> 
                []
            Just stock ->
                [ text stock.name ]
        )

view model =
    div [] 
        [ viewNav model
        , div [ Attributes.class "flex one three-600" ]
            [ div [ Attributes.class "fifth-600 fourth-1000" ] []
            , div [ Attributes.class "three-fifth-600 half-1000" ] 
                [ case model.route of
                    Route.Protected Route.MyPositions ->
                        viewMyPositions model
                    
                    Route.Protected (Route.StockPosition id) ->
                        viewStockPosition model
                    
                    Route.Public Route.SignIn ->
                        viewSignIn model
                        
                    Route.Public Route.NotFound ->
                        viewNotFound model
                ]
            , div [ Attributes.class "fifth-600 fourth-1000" ] []
            ]
        ]

main 
    = Navigation.program (Route.parse >> RouteChange)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        } 