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
    
model = Model "" "" Nothing

update msg model = 
    case msg of
        TypeEmail email ->
            { model | email = email }
        TypePassword password ->
            { model | password = password }
        SubmitCredentials ->
            model

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
    = Html.beginnerProgram {
        model = model,
        update = update,
        view = view
    } 