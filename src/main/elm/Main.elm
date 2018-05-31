import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
    { init = start
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL

type alias Model =
  { count : Int
  , message : String
  , member : Maybe Member
  , memberID : Int
  , memberName : String
  , memberEmail : String
  }

type alias Member = {
  id: Int,
  name: String,
  email: String
}

start : (Model, Cmd Msg)
start =
  ( Model 0 "No Errors" Nothing -1 "" ""
  , Cmd.none
  )

-- UPDATE

type Msg
  = GetMemberCount
  | GetMember
  | PostMember
  | PostMemberReceived (Result Http.Error Member)
  | UpdateNumber String
  | UpdateName String
  | UpdateEmail String
  | MemberReceived (Result Http.Error Member)
  | MemberCountReceived (Result Http.Error Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMember -> (model, getMember model.memberID)
    MemberReceived (Ok member) -> ({ model | member = Just member, memberName = member.name, memberEmail = member.email }, Cmd.none)
    MemberReceived (Err error) -> ({ model | message = toString error }, Cmd.none)

    GetMemberCount -> (model, getMemberCount)
    MemberCountReceived (Ok newCount) -> ({ model | count = newCount }, Cmd.none)
    MemberCountReceived (Err error) -> ({ model | message = toString error }, Cmd.none)

    UpdateNumber string -> ({ model | memberID = stringToInt string }, Cmd.none)
    UpdateName string -> ({ model | memberName = string }, Cmd.none)
    UpdateEmail string -> ({ model | memberEmail = string }, Cmd.none)

    PostMember -> (model, postMember (Member model.memberID model.memberName model.memberEmail))
    PostMemberReceived (Ok ok) -> ({ model | member = Just ok, message = memberToString ok }, getMemberCount)
    PostMemberReceived (Err err) -> ({ model | message = toString err }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , hr [] []
    , text model.message
    , hr [] []
    , input [type_ "number", placeholder "id", onInput UpdateNumber] []
    , br [] []
    , input [type_ "text", placeholder "name", value model.memberName, onInput UpdateName] []
    , input [type_ "text", placeholder "email", value model.memberEmail, onInput UpdateEmail] []
    , button [onClick GetMember] [text "Get Member"]
    , button [onClick PostMember] [text "Post Member"]
    --, text (memberToString model.member)
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

getMember : Int -> Cmd Msg
getMember id = Http.send MemberReceived (Http.get (url (toString id)) decodeMember)

postMember : Member -> Cmd Msg
postMember member =
  Http.send PostMemberReceived (Http.post (url "/") (jsonBody (encodeMember member)) decodeMember)

decodeMember : Decode.Decoder Member
decodeMember =
  Decode.map3 Member
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)

encodeMember : Member -> Encode.Value
encodeMember member =
  Encode.object
  [
    ("id", Encode.int member.id),
    ("name", Encode.string member.name),
    ("email", Encode.string member.email)
  ]

-- Other stuff

memberToString : Member -> String
memberToString m = "Id: " ++ (toString m.id) ++ ", Name: " ++ m.name ++ ", Email: " ++ m.email

stringToInt : String -> Int
stringToInt str =
  case (String.toInt str) of
    Ok x ->  x
    Err _ -> -1
