module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, href)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)
import Model.Date as Date exposing(..)

type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"

compareEvInt : Event -> Event -> Order
compareEvInt evA evB = 
    let 
        intA = evA.interval
        intB = evB.interval
    in
        Interval.compare intA intB

sortByInterval : List Event -> List Event
sortByInterval events =
    List.sortWith compareEvInt events


view : Event -> Html Never
view event =
     div [Attr.class (if event.important then "event event-important" else "event")] [
        h3 [Attr.class "event-title"] [text event.title],
        p [Attr.class "event-description"] [event.description],
        p [Attr.class "event-category"] [categoryView event.category],
        p [Attr.class "event-interval"] [text ("Start: " ++ Date.dateToString (Interval.getStart event.interval) ++ " End: " ++ Date.dateToString (Maybe.withDefault (Interval.getStart event.interval) (Interval.getEnd event.interval)))],
        a [href (Maybe.withDefault "" event.url), Attr.class "event-url"] [text (Maybe.withDefault "" event.url)]
     ]

