module Main exposing (main)

import Browser
import Html exposing (Html, label, button, span, i, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex

emailPattern : Regex.Regex
emailPattern =  Maybe.withDefault Regex.never <|
    Regex.fromString "^(([^<>()\\[\\]\\.,;:\\s@\"]+(\\.[^<>()\\[\\]\\.,;:\\s@\"]+)*)|(\".+\"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$"

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model = { name : String, email : String, error: Bool }

init : Model
init =
  Model "" "" False

type Msg = Name String | Email String | Check

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Email email ->
      { model | email = email }

    Check -> { model | error = True }

view : Model -> Html Msg
view model =
  div [] [
    div [ class "field" ] [ label [ class "label" ] [ text "Your Name" ], div [ class "control has-icons-left has-icons-right" ] [ viewInput "text" "Please type your name" model.name Name "input", span [ class "icon is-small is-left" ] [ i [ class "fas fa-user" ] [] ] ],
    div [ class "field" ] [ label [ class "label" ] [ text "Your Email" ],  div [ class "control has-icons-left has-icons-right" ] [ viewInput "email" "Please type your email" model.email Email "input", span [ class "icon is-small is-left" ] [ i [ class "fas fa-envelope" ] [] ]]],
    viewValidation model,
    button [ class "button is-link", onClick Check ] [ text "Check" ]
     ]]

viewInput : String -> String -> String -> (String -> msg) -> String -> Html msg
viewInput t p v toMsg classNm =
  input [ type_ t, placeholder p, value v, onInput toMsg, class classNm ] []
  
viewValidation : Model -> Html msg
viewValidation model =
    if model.error == True then
        if Regex.contains emailPattern model.email == True then
            div [ class "help is-success" ] [ text "Email is valid" ]
        else
            div [ class "help is-danger" ] [ text "Email is not valid" ]
    else
        div [] []