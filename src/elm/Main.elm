import Html exposing (Html, div, form, input)
import Html.Attributes as Attr


type alias Model = 
    { username: String
    , password: String
    , token: Maybe String
    }
    
type Msg 
    = TypeUsername String
    | TypePassword String
    | SignIn
    
model = Model "" "" Nothing

update msg model = 
    case msg of
        TypeUsername username ->
            { model | username = username }
        TypePassword password ->
            { model | password = password }
        SignIn ->
            model

view model =
    div [] 
        [ form [] 
            [ input [Attr.type_ "text"] []
            ]
        ]

main 
    = Html.beginnerProgram {
        model = model,
        update = update,
        view = view
    } 