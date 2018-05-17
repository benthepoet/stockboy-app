port module Main exposing (..)

-- IMPORTS

import Html exposing (Html, a, article, button, div, footer, form, h1, h3, h4, header, i, input, label, nav, p, section, span, table, text, tr, td)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Navigation
import Request
import Route
import Storage
import Task
import Time
import Util
import Widgets


-- TYPES


type alias Flags =
    { token : Maybe String
    }


type alias Model =
    { balance : Float
    , confirmPassword : String
    , email : String
    , equity : Float
    , modalState : ModalState
    , password : String
    , positions : List Request.Position
    , refreshInterval : Time.Time
    , route : Route.Route
    , stock : Maybe Request.Stock
    , stocks : List Request.Stock
    , token : Maybe String
    }


type ModalState
    = Buy
    | Empty
    | Error
    | Sell


type Msg
    = DismissModal
    | Home
    | LoadOrder (Result Http.Error Request.Order)
    | LoadPositions (Result Http.Error (List Request.Position))
    | LoadStock (Result Http.Error Request.Stock)
    | LoadStocks (Result Http.Error (List Request.Stock))
    | LoadToken (Result Http.Error Request.AuthResponse)
    | LoadUser (Result Http.Error Request.User)
    | RefreshData Time.Time
    | RouteChange Route.Route
    | Search
    | ShowModal ModalState
    | SignOut
    | SignUp
    | SubmitSignIn
    | SubmitSignUp
    | TypeConfirmPassword String
    | TypeEmail String
    | TypePassword String
    | TypeSearch String
    | ViewStock Int



-- PROGRAM


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    let
        refreshInterval =
            10 * 1000

        route =
            Route.parse location
    in
        ( Model 0 "" "" 0 Empty "" [] refreshInterval route Nothing [] flags.token
        , Cmd.batch
            [ Task.perform RouteChange (Task.succeed route)
            , forceRefresh
            ]
        )


calculateEquity =
    List.sum << (List.map (\n -> (toFloat n.totalUnits) * n.stock.lastPrice))


findPosition id =
    List.head << (List.filter ((==) id << .id << .stock))


forceRefresh =
    Task.perform RefreshData Time.now


update msg model =
    case msg of
        DismissModal ->
            ( { model | modalState = Empty }
            , Cmd.none
            )

        Home ->
            ( model
            , Navigation.newUrl (Route.toPath <| Route.Protected Route.MyPositions)
            )

        LoadOrder (Ok order) ->
            ( model, Cmd.none )

        LoadOrder (Err _) ->
            ( { model | modalState = Error }
            , Cmd.none
            )

        LoadPositions (Ok positions) ->
            ( { model
                | equity = calculateEquity positions
                , positions = positions
              }
            , Cmd.none
            )

        LoadPositions (Err _) ->
            ( { model | modalState = Error }
            , Cmd.none
            )

        LoadStock (Ok stock) ->
            ( { model | stock = Just stock }
            , Cmd.none
            )

        LoadStock (Err _) ->
            ( { model | modalState = Error }
            , Cmd.none
            )

        LoadStocks (Ok stocks) ->
            ( { model | stocks = stocks }
            , Cmd.none
            )

        LoadStocks (Err _) ->
            ( { model | modalState = Error }
            , Cmd.none
            )

        LoadToken (Ok response) ->
            let
                token =
                    Just response.token
            in
                ( { model | token = token }
                , Cmd.batch
                    [ Navigation.newUrl (Route.toPath <| Route.Protected Route.MyPositions)
                    , forceRefresh
                    , Storage.syncToken token
                    ]
                )

        LoadToken (Err _) ->
            ( { model | modalState = Error }
            , Cmd.none
            )

        LoadUser (Ok user) ->
            ( { model | balance = user.balance }
            , Cmd.none
            )

        LoadUser (Err _) ->
            ( { model | modalState = Error }
            , Cmd.none
            )

        RefreshData time ->
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

        ShowModal modalState ->
            ( { model | modalState = modalState }
            , Cmd.none
            )

        SignOut ->
            let
                token =
                    Nothing
            in
                ( { model | token = token }
                , Cmd.batch
                    [ Navigation.newUrl (Route.toPath <| Route.Public Route.SignIn)
                    , Storage.syncToken token
                    ]
                )

        SignUp ->
            ( model
            , Navigation.newUrl (Route.toPath <| Route.Public Route.SignUp)
            )

        SubmitSignIn ->
            ( model
            , Http.send LoadToken <| Request.authenticate model.email model.password
            )

        SubmitSignUp ->
            ( model
            , Http.send LoadToken <| Request.signUp model.email model.password
            )

        TypeConfirmPassword password ->
            ( { model | confirmPassword = password }
            , Cmd.none
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
    Time.every model.refreshInterval RefreshData


viewModal model =
    div [ Attributes.class "modal" ]
        [ input
            [ Attributes.name "main-modal"
            , Attributes.type_ "checkbox"
            , Attributes.checked <| model.modalState /= Empty
            ]
            []
        , label
            [ Attributes.for "main-modal"
            , Attributes.class "overlay"
            ]
            []
        , case model.modalState of
            Buy ->
                article []
                    [ header []
                        [ h3 [] [ text "Buy" ]
                        ]
                    , footer []
                        [ a
                            [ Attributes.class "button"
                            , Events.onClick DismissModal
                            ]
                            []
                        ]
                    ]

            Empty ->
                article [] []

            Error ->
                article []
                    [ header []
                        [ h3 [] [ text "Warning" ]
                        ]
                    , section [ Attributes.class "content" ]
                        [ text "An unexpected error occurred." ]
                    , footer []
                        [ a
                            [ Attributes.class "button"
                            , Events.onClick DismissModal
                            ]
                            [ text "Dismiss" ]
                        ]
                    ]

            Sell ->
                article [] []
        ]


viewMyPositions model =
    div [ Attributes.class "row" ]
        [ article [ Attributes.class "card shadow my-account" ]
            [ header [] [ text "My Account" ]
            , section [ Attributes.class "padding" ]
                [ Widgets.staticField "Equity Balance" (Util.formatCurrency model.equity)
                , Widgets.staticField "Cash Balance" (Util.formatCurrency model.balance)
                , Widgets.staticField "Account Value" (Util.formatCurrency (model.equity + model.balance))
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
                , label [ Attributes.for "bmenub", Attributes.class "button burger" ]
                    [ i [ Attributes.class "fas fa-bars" ] []
                    ]
                , div [ Attributes.class "menu" ]
                    [ Widgets.menuButton "Search" "search" Search
                    , Widgets.menuButton "My Portfolio" "home" Home
                    , Widgets.menuButton "My Profile" "user" SignOut
                    , Widgets.menuButton "Sign Out" "sign-out-alt" SignOut
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


viewPositionBuy model =
    div [ Attributes.class "modal" ]
        [ input
            [ Attributes.name "buy-modal"
            , Attributes.type_ "checkbox"
            , Attributes.checked model.modalState
            ]
            []
        , label
            [ Attributes.for "buy-modal"
            , Attributes.class "overlay"
            ]
            []
        , article []
            [ header []
                [ h3 [] [ text "Buy Stock" ]
                ]
            , section [ Attributes.class "content" ]
                [ text "An unexpected error occurred." ]
            , footer []
                [ a
                    [ Attributes.class "button"
                    , Events.onClick DismissModal
                    ]
                    [ text "Dismiss" ]
                ]
            ]
        ]


viewStockList model =
    div [ Attributes.class "row" ]
        [ article [ Attributes.class "card shadow stock-list" ]
            [ header []
                [ form []
                    [ input
                        [ Attributes.type_ "text"
                        , Attributes.placeholder "Type to search"
                        , Events.onInput TypeSearch
                        ]
                        []
                    ]
                ]
            , section [ Attributes.class "no-padding" ]
                [ table []
                    (List.map viewStockListItem model.stocks)
                ]
            ]
        ]


viewStockListItem stock =
    tr [ Events.onClick (ViewStock stock.id) ]
        [ td []
            [ h4 [ Attributes.class "no-padding" ] [ text stock.symbol ]
            , text stock.name
            ]
        ]


viewSignIn model =
    div []
        [ h3 [] [ text "Sign In" ]
        , form [ Events.onSubmit SubmitSignIn ]
            [ input
                [ Attributes.class "stack"
                , Attributes.placeholder "Email"
                , Attributes.type_ "text"
                , Events.onInput TypeEmail
                ]
                []
            , input
                [ Attributes.class "stack"
                , Attributes.placeholder "Password"
                , Attributes.type_ "password"
                , Events.onInput TypePassword
                ]
                []
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
                , Events.onClick SignUp
                ]
                [ i [ Attributes.class "fas fa-user-plus" ] []
                , text "Sign Up"
                ]
            ]
        ]


viewSignUp model =
    div []
        [ h3 [] [ text "Create Account" ]
        , form [ Events.onSubmit SubmitSignUp ]
            [ input
                [ Attributes.class "stack"
                , Attributes.placeholder "Email"
                , Attributes.type_ "text"
                , Events.onInput TypeEmail
                ]
                []
            , input
                [ Attributes.class "stack"
                , Attributes.placeholder "Password"
                , Attributes.type_ "password"
                , Events.onInput TypePassword
                ]
                []
            , input
                [ Attributes.class "stack"
                , Attributes.placeholder "Confirm Password"
                , Attributes.type_ "password"
                , Events.onInput TypeConfirmPassword
                ]
                []
            , button
                [ Attributes.class "stack"
                , Attributes.type_ "submit"
                ]
                [ i [ Attributes.class "fas fa-user-plus" ] []
                , text "Create Account"
                ]
            ]
        ]


viewStockPosition model =
    div [ Attributes.class "row" ] <|
        case model.stock of
            Nothing ->
                []

            Just stock ->
                let
                    position =
                        findPosition stock.id model.positions
                in
                    [ div [ Attributes.class "card shadow" ]
                        [ header []
                            [ h3 [] [ text stock.name ]
                            ]
                        , section []
                            (case position of
                                Nothing ->
                                    []

                                Just _ ->
                                    [ button [] [ text "Sell" ]
                                    ]
                            )
                        ]
                    ]


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

                    Route.Public Route.SignUp ->
                        viewSignUp model

                    Route.Public Route.NotFound ->
                        viewNotFound model
                ]
            , div [ Attributes.class "fifth-600 fourth-1000" ] []
            ]
        , viewModal model
        ]


main =
    Navigation.programWithFlags (Route.parse >> RouteChange)
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
