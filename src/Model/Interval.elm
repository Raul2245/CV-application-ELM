module Model.Interval exposing (Interval, compare, full, length, oneYear, open, view, withDurationMonths, withDurationYears, getStart, getEnd)

import Html exposing (Html, div, p, text)
import Html.Attributes as Attr exposing (class, style)
import Model.Date as Date exposing (Date, Month, dateToString)
import Model.Util exposing (chainCompare)


type Interval
    = Interval { start : Date, end : Maybe Date }


getStart : Interval -> Date
getStart (Interval interval) = interval.start

getEnd : Interval -> Maybe Date
getEnd (Interval interval) = interval.end

{-| Create an `Interval` from 2 `Date`s. If the second date is before the first the date, the function will return
`Nothing`. When possible, use the `withDurationMonths` or `withDurationYears` functions.
-}
full : Date -> Date -> Maybe Interval
full start end =
    if Date.compare start end == GT then
        Nothing

    else
        Just <| Interval { start = start, end = Just end }


{-| Create an `Interval` from a start year, start month, and a duration in months.
The start year and month are explicitly required because the duration in months is only specified if the start date
also includes a month.
This function, (assuming positive inputs) by definition, can always return a valid `Interval`.
-}
withDurationMonths : Int -> Month -> Int -> Interval
withDurationMonths startYear startMonth duration =
    let
        start =
            Date.full startYear startMonth

        end =
            Date.offsetMonths duration start
    in
    Interval { start = start, end = Just end }


{-| Create an `Interval` from a start `Date`, and a duration in years. This function, (assuming positive inputs)
by definition, can always return a valid `Interval`.
-}
withDurationYears : Date -> Int -> Interval
withDurationYears start duration =
    let
        end =
            Date.offsetMonths (duration * 12) start
    in
    Interval { start = start, end = Just end }


{-| Create an open `Interval` from a start `Date`. Usually used for creating ongoing events.
-}
open : Date -> Interval
open start =
    Interval { start = start, end = Nothing }


{-| Convenience function to create an `Interval` that represents one year.
-}
oneYear : Int -> Interval
oneYear year =
    withDurationYears (Date.onlyYear year) 1


{-| The length of an Interval, in (years, months)
-}
length : Interval -> Maybe ( Int, Int )
length (Interval interval) =
    interval.end
        |> Maybe.andThen (Date.monthsBetween interval.start)
        |> Maybe.map (\totalMonths -> ( totalMonths // 12, modBy 12 totalMonths ))


{-| Compares two intervals.

Intervals are first compared compare by the `start` field.
If the `start` field is equal, the they are compare by the `end` fields:

  - If both are missing (`Nothing`), the intervals are considered equal
  - If both are present (`Just`), the longer interval is considered greater
  - If only one interval is open (its `end` field is `Nothing`) then it will be considered greater

```
    import Model.Date as Date

    Model.Interval.compare (oneYear 2019) (oneYear 2020) --> LT
    Model.Interval.compare (oneYear 2019) (withDurationYears (Date.onlyYear 2020) 2) --> LT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (withDurationMonths 2019 Date.Jan 2) --> EQ
    Model.Interval.compare (withDurationMonths 2019 Date.Feb 2) (withDurationMonths 2019 Date.Jan 2) --> GT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (open (Date.onlyYear 2019)) --> LT
```

-}
compare : Interval -> Interval -> Order
compare (Interval intA) (Interval intB) =
     case (Date.compare (intA.start) (intB.start)) of
        GT -> GT
        LT -> LT
        EQ -> case intA.end of  
                    Nothing -> case intB.end of
                                    Nothing -> EQ
                                    Just _ -> GT
                    Just _ -> case intB.end of
                                    Nothing -> LT
                                    Just _ -> Date.compare (Maybe.withDefault intA.start intA.end) (Maybe.withDefault intB.start intB.end)


intervalToString: (Int, Int) -> String
intervalToString (years, months) =
    String.fromInt months ++ " months, " ++ String.fromInt years ++ " years"

view : Interval -> Html msg
view (Interval interval) =
     div [Attr.class "interval"] [
        p [Attr.class "interval-start"] [text ("Start: " ++ (Date.dateToString interval.start))],
        p [Attr.class "interval-end"] [text ("End: " ++ (case interval.end of 
                                                                    Nothing -> " Present"
                                                                    Just _ -> Date.dateToString (Maybe.withDefault interval.start interval.end)))] ,
        p [Attr.class "interval-length"] 
            [text 
            <| intervalToString 
            <| Maybe.withDefault (0, 0) 
            <| length (Interval interval)
            ]                                                            
      ]
