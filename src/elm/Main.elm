module Main exposing (..)

-- IMPORTS

import Html exposing (Html, a, article, button, div, footer, form, h1, h3, h4, header, i, input, label, nav, p, section, span, table, text, tr, td)
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
    , error: Bool
    , password: String
    , pollInterval: Time.Time
    , positions: List Request.PositionResponse
    , route: Route.Route
    , stock: Maybe Request.Stock
    , stocks: List Request.Stock
    , token: Maybe String
    }
    
type Msg 
    = DismissError
    | Home
    | LoadPositions (Result Http.Error (List Request.PositionResponse))
    | LoadStock (Result Http.Error Request.Stock)
    | LoadStocks (Result Http.Error (List Request.Stock))
    | LoadToken (Result Http.Error Request.AuthResponse)
    | LoadUser (Result Http.Error Request.User)
    | Poll Time.Time
    | RouteChange Route.Route
    | Search
    | SignOut
    | SubmitCredentials
    | TypeEmail String
    | TypePassword String
    | TypeSearch String
    | ViewStock Int
    
-- PROGRAM
    
init location =
    let
        pollInterval = 10 * 1000
        route = Route.parse location
    in
        ( Model 0 "" 0 False "" pollInterval [] route Nothing [] Nothing
        , Task.perform RouteChange (Task.succeed route))

calculateEquity positions =
    positions
        |> List.map (\n -> (toFloat n.totalUnits) * n.stock.lastPrice)
        |> List.sum

update msg model = 
    case msg of
        DismissError ->
            ( { model | error = False }
            , Cmd.none
            )
    
        Home ->
            ( model
            , Navigation.newUrl (Route.toPath <| Route.Protected Route.MyPositions)
            )
    
        LoadPositions (Ok positions) ->
            ( { model 
                | equity = calculateEquity positions
                , positions = positions }
            , Cmd.none 
            )
        
        LoadPositions (Err _) ->
            ( { model | error = True }
            , Cmd.none 
            )
        
        LoadStock (Ok stock) ->
            ( { model | stock = Just stock }
            , Cmd.none
            )
            
        LoadStock (Err _) ->
            ( { model | error = True }
            , Cmd.none 
            )
            
        LoadStocks (Ok stocks) ->
            ( { model | stocks = stocks }
            , Cmd.none
            )
            
        LoadStocks (Err _) ->
            ( { model | error = True }
            , Cmd.none 
            )
        
        LoadToken (Ok authResponse) ->
            ( { model | token = Just authResponse.token }
            , Cmd.batch 
                [ Navigation.newUrl (Route.toPath <| Route.Protected Route.MyPositions)
                , Task.perform Poll Time.now
                ]
            )        
        
        LoadToken (Err _) ->
            ( { model | error = True }
            , Cmd.none 
            )
            
        LoadUser (Ok user) ->
            ( { model | balance = user.balance }
            , Cmd.none
            )
        
        LoadUser (Err _) ->
            ( { model | error = True }
            , Cmd.none 
            )
            
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
        
        Search ->
            ( model
            , Navigation.newUrl (Route.toPath <| Route.Protected Route.StockList)
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
            
        TypeSearch search ->
            ( model
            , Http.send LoadStocks (Request.getStocks (Maybe.withDefault "" model.token) search)
            )
            
        ViewStock id ->
            ( model
            , Navigation.newUrl (Route.toPath <| Route.Protected (Route.StockPosition id))
            )

subscriptions model = 
    Time.every model.pollInterval Poll

viewError model =
    div [ Attributes.class "modal" ]
        [ input 
            [ Attributes.name "error-modal"
            , Attributes.type_ "checkbox"
            , Attributes.checked model.error 
            ] []
        , label 
            [ Attributes.for "error-modal"
            , Attributes.class "overlay"
            ] []
        , article []
            [ header [] 
                [ h3 [] [ text "Warning" ]
                ]
            , section [ Attributes.class "content" ]
                [ text "An unexpected error occurred." ]
            , footer []
                [ a 
                    [ Attributes.class "button"
                    , Events.onClick DismissError 
                    ] [ text "Dismiss" ]
                ]
            ]
        ]

viewMyPositions model =
    div [ Attributes.class "row" ] 
        [ article [ Attributes.class "card shadow my-account" ]
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
    case model.token of
        Nothing ->
            div [] []
        Just token ->
            nav [] 
                [ input [ Attributes.id "bmenub", Attributes.type_ "checkbox", Attributes.class "show" ] []
                , label [ Attributes.for "bmenub", Attributes.class "button burger" ] [ text "menu" ]
                , div [ Attributes.class "menu" ]
                    [ a 
                        [ Attributes.class "button"
                        , Events.onClick Home ] 
                        [ i [ Attributes.class "fas fa-home" ] []
                        , text "My Portfolio" 
                        ]
                    , a 
                        [ Attributes.class "button"
                        , Events.onClick Search ] 
                        [ i [ Attributes.class "fas fa-search" ] []
                        , text "Search" 
                        ]
                    , a 
                        [ Attributes.class "button"
                        , Events.onClick SignOut ] 
                        [ i [ Attributes.class "fas fa-user" ] []
                        , text "My Profile" 
                        ]
                    , a 
                        [ Attributes.class "button"
                        , Events.onClick SignOut ] 
                        [ i [ Attributes.class "fas fa-sign-out-alt" ] []
                        , text "Sign Out" 
                        ] 
                    ]
                ]

viewNotFound model =
    div [] [ text "Not Found" ]
    
viewPosition position =
    article [ Attributes.class "card shadow position", Events.onClick (ViewStock position.stock.id) ]
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

viewStockList model =
    div []
        [ form []
            [ input 
                [ Attributes.type_ "text"
                , Attributes.placeholder "Type to search"
                , Events.onInput TypeSearch 
                ] []
            ]
        , table []
            (List.map viewStockListItem model.stocks)
        ]
        
viewStockListItem stock =
    tr []
        [ td [] [ text stock.symbol ] ]
    
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
                [ i [ Attributes.class "fas fa-sign-in-alt" ] [] 
                , text "Sign In" 
                ]
            ]
        , div [] 
            [ p [] [ text "Not a member?" ]
            , button 
                [ Attributes.class "button" 
                , Attributes.type_ "button"
                ] 
                [ i [ Attributes.class "fas fa-user-plus" ] []
                , text "Create Account" 
                ]
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
                    
                    Route.Protected Route.StockList ->
                        viewStockList model
                    
                    Route.Public Route.SignIn ->
                        viewSignIn model
                        
                    Route.Public Route.NotFound ->
                        viewNotFound model
                ]
            , div [ Attributes.class "fifth-600 fourth-1000" ] []
            ]
        , viewError model
        ]

main 
    = Navigation.program (Route.parse >> RouteChange)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        } 