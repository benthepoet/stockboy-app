import Html exposing (beginnerProgram, div, form, h3, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


type alias Model = 
    { message: String
    }
    
type Msg 
    = TypeMessage String
    
model = Model "Elm Webpack Starter"

update msg model = 
    case msg of
        TypeMessage message ->
            { model | message = message }

view model =
    div [] 
        [ h3 [] [text model.message]
        , form [] 
            [ input 
                [ type_ "text"
                , onInput TypeMessage
                , value model.message
                ] []
            ]
        ]

main 
    = beginnerProgram {
        model = model,
        update = update,
        view = view
    } 