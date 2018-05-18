import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode


main : Program Never Model Msg
main =
  Html.program
    { 
      init = start,
      view = view,
      update = update,
      subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL

type alias Model =
  { count : Int
  , message : String
  , member : Maybe Member
  , memberID: Maybe Int
  }

type alias Member =
  { id: Int
  , name: String
  , email: String
  }

start : (Model, Cmd Msg)
start = (Model 0 "No message" Nothing Nothing
        , Cmd.none)

-- UPDATE

type Msg
  = GetMemberCount
  | GetMember
  | UpdateNumber String
  | MemberCountReceived (Result Http.Error Int)
  | MemberRecieved (Result Http.Error Member)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount -> (model, getMemberCount)
    MemberCountReceived (Ok newCount) -> ( { model | count = newCount }, Cmd.none)
    MemberCountReceived (Err error) -> ( { model | message = toString error }, Cmd.none)

    GetMember -> (model, getMember model.memberID)
    MemberRecieved (Ok member) -> ( { model | member = Just member }, Cmd.none)
    MemberRecieved (Err error) -> ( { model | message = toString error }, Cmd.none)

    UpdateNumber string -> ( { model | memberID = stringToInt string  }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , hr [] []
    , text model.message
    , hr [] []
    , input [type_ "number", onInput UpdateNumber] []
    , button [ onClick GetMember ] [ text "Get Member"]
    , text (memberToString model.member)
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

getMember :Maybe Int -> Cmd Msg
getMember id =
    case id of
    Just int -> Http.send MemberRecieved (Http.get (url (toString int)) decodeMember)
    Nothing -> Cmd.none

decodeMember : Decode.Decoder Member
decodeMember =
    Decode.map3 Member
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "email" Decode.string)

encodeMember: Member -> Encode.Value
encodeMember member =
   Encode.object 
    [ ("id", Encode.int member.id)
    , ("name", Encode.string member.name)
    , ("email", Encode.string member.email)
    ]


-- Helpers

memberToString : Maybe Member -> String
memberToString member =
    case member of
        Just m -> "ID: " ++ (toString m.id) ++ ", Name: " ++ m.name ++ ", Email: " ++ m.email
        Nothing -> "No Member"

stringToInt : String -> Maybe Int
stringToInt str = 
    case (String.toInt str) of
      Ok x -> Just x
      Err _ -> Nothing