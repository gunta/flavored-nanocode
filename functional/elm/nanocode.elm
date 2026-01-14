-- nanocode - minimal claude code alternative (Elm)
-- Note: Elm is designed for web apps, this is a browser-based version
-- elm make nanocode.elm --output=nanocode.js
module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Encode as E

type alias Model = { input : String, messages : List Message, history : List (String, String) }
type alias Message = { role : String, content : String }
type Msg = Input String | Submit | GotResponse (Result Http.Error String)

main = Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }

init : () -> (Model, Cmd Msg)
init _ = ({ input = "", messages = [], history = [] }, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input s -> ({ model | input = s }, Cmd.none)
        Submit ->
            if model.input == "/c" then
                ({ model | input = "", messages = [], history = ("system", "Cleared") :: model.history }, Cmd.none)
            else
                let newMessages = model.messages ++ [{ role = "user", content = model.input }]
                in ({ model | input = "", messages = newMessages, history = ("user", model.input) :: model.history }, askApi newMessages)
        GotResponse (Ok resp) ->
            ({ model | history = ("assistant", resp) :: model.history }, Cmd.none)
        GotResponse (Err _) ->
            ({ model | history = ("error", "API error") :: model.history }, Cmd.none)

askApi : List Message -> Cmd Msg
askApi messages =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Content-Type" "application/json", Http.header "anthropic-version" "2023-06-01" ]
        , url = "https://api.anthropic.com/v1/messages"
        , body = Http.jsonBody (encodeRequest messages)
        , expect = Http.expectString GotResponse
        , timeout = Nothing
        , tracker = Nothing
        }

encodeRequest : List Message -> E.Value
encodeRequest messages =
    E.object
        [ ("model", E.string "claude-sonnet-4-20250514")
        , ("max_tokens", E.int 4096)
        , ("messages", E.list encodeMessage messages)
        ]

encodeMessage : Message -> E.Value
encodeMessage m = E.object [("role", E.string m.role), ("content", E.string m.content)]

view : Model -> Html Msg
view model =
    div [ style "font-family" "monospace", style "background" "#0d1117", style "color" "#c9d1d9", style "min-height" "100vh", style "padding" "20px" ]
        [ h2 [] [ text "🧬 nanocode (Elm)" ]
        , div [ style "white-space" "pre-wrap" ] (List.map viewHistory (List.reverse model.history))
        , Html.form [ onSubmit Submit, style "display" "flex" ]
            [ span [ style "color" "#58a6ff" ] [ text "❯ " ]
            , input [ value model.input, onInput Input, style "flex" "1", style "background" "#21262d", style "border" "1px solid #30363d", style "color" "#c9d1d9", style "padding" "8px" ] []
            , button [ style "background" "#238636", style "color" "white", style "border" "none", style "padding" "8px 16px" ] [ text "Send" ]
            ]
        ]

viewHistory : (String, String) -> Html Msg
viewHistory (role, content) =
    div [] [ span [ style "color" (if role == "user" then "#58a6ff" else "#3fb950") ] [ text (if role == "user" then "❯ " else "⏺ ") ], text content ]
