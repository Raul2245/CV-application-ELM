module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, href)
import Json.Decode as De


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


view : Repo -> Html msg
view repo =
     div [Attr.class "repo"] [
      h3 [Attr.class "repo-name"] [text repo.name],
      p [Attr.class "repo-description"] [text (Maybe.withDefault "No Description" repo.description)],
      div [Attr.class "repo-url"] [a [Attr.href repo.url] [text repo.url]],
      p [Attr.class "repo-stars"] [text ("Stars: " ++ (String.fromInt repo.stars))]
     ]


sortByStars : List Repo -> List Repo
sortByStars repos = 
     List.sortBy .stars repos


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}
decodeRepo : De.Decoder Repo
decodeRepo =
     De.map5 Repo
          (De.at ["name"] De.string)
          (De.maybe (De.at ["description"] De.string))
          (De.at ["html_url"] De.string)
          (De.at ["pushed_at"] De.string)
          (De.at ["stargazers_count"] De.int)
    --Debug.todo "Implement Model.Repo.decodeRepo"
