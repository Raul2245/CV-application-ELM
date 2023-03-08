module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, href)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }

returnContacts: List DetailWithName -> List (Html msg) -> List (Html msg)
returnContacts d htm =
    case d of  
        [] -> htm
        x :: xs -> returnContacts xs ((p [Attr.class "contact-detail"] [text (x.name ++ ": " ++ x.detail)]) :: htm)

returnSocials: List DetailWithName -> List (Html msg) -> List (Html msg)
returnSocials d htm =
    case d of  
        [] -> htm
        x :: xs -> returnSocials xs (p [] [text (x.name ++ ": ")] :: ((a [href x.detail, Attr.class "social-link"] [text x.detail]) :: htm))

view : PersonalDetails -> Html msg
view details =
     div [] 
        ((h1 [(Attr.id "name")] [text details.name] :: 
         (em [(Attr.id "intro")] [text details.intro] ::
        (returnContacts details.contacts []))) ++
        (returnSocials details.socials []))
