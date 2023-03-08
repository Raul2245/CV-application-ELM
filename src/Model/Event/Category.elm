module Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected, eventCategories, isEventCategorySelected, set, view, replaceList, stringToEvent, eventToString)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck)


type EventCategory
    = Academic
    | Work
    | Project
    | Award


eventCategories =
    [ Academic, Work, Project, Award ]


{-| Type used to represent the state of the selected event categories
-}
type SelectedEventCategories =
    AcademicEventSelected | WorkEventSelected | ProjectEventSelected | AwardEventSelected | AllEventsSelected | NoEventsSelected |
    AcademicWorkEventSelected | AcademicProjectEventSelected | AcademicAwardEventSelected | 
    WorkProjectEventSelected | WorkAwardEventSelected | ProjectAwardEventSelected |
    AcademicWorkProjectEventSelected | AcademicWorkAwardEventSelected | WorkProjectAwardEventSelected | AcademicProjectAwardEventSelected

{-| Returns an instance of `SelectedEventCategories` with all categories selected

    isEventCategorySelected Academic allSelected --> True

-}
allSelected : SelectedEventCategories
allSelected =
    AllEventsSelected
    --Debug.todo "Implement Model.Event.Category.allSelected"

{-| Returns an instance of `SelectedEventCategories` with no categories selected

-- isEventCategorySelected Academic noneSelected --> False

-}
noneSelected : SelectedEventCategories
noneSelected =
    NoEventsSelected

{-| Given a the current state and a `category` it returns whether the `category` is selected.

    isEventCategorySelected Academic allSelected --> True

-}
isEventCategorySelected : EventCategory -> SelectedEventCategories -> Bool
isEventCategorySelected category current =
     case category of 
        Academic -> case current of 
                        AcademicEventSelected -> True
                        AcademicWorkEventSelected -> True
                        AcademicProjectEventSelected -> True
                        AcademicAwardEventSelected -> True
                        AcademicWorkProjectEventSelected -> True
                        AcademicWorkAwardEventSelected -> True
                        AcademicProjectAwardEventSelected -> True
                        AllEventsSelected -> True
                        _ -> False
        Work -> case current of 
                        WorkEventSelected -> True
                        AcademicWorkEventSelected -> True
                        WorkAwardEventSelected -> True
                        WorkProjectEventSelected -> True
                        WorkProjectAwardEventSelected -> True
                        AcademicWorkAwardEventSelected -> True
                        AcademicWorkProjectEventSelected -> True
                        AllEventsSelected -> True
                        _ -> False
        Project -> case current of 
                        ProjectEventSelected -> True
                        AcademicProjectEventSelected -> True
                        WorkProjectEventSelected -> True
                        ProjectAwardEventSelected -> True
                        AcademicWorkProjectEventSelected -> True
                        AcademicProjectAwardEventSelected -> True
                        WorkProjectAwardEventSelected -> True
                        AllEventsSelected -> True
                        _ -> False
        Award -> case current of 
                        AwardEventSelected -> True
                        AcademicAwardEventSelected -> True
                        WorkAwardEventSelected -> True
                        ProjectAwardEventSelected -> True
                        AcademicWorkAwardEventSelected -> True
                        AcademicProjectAwardEventSelected -> True
                        WorkProjectAwardEventSelected -> True
                        AllEventsSelected -> True
                        _ -> False
    --Debug.todo "Implement Model.Event.Category.isEventCategorySelected"


{-| Given an `category`, a boolean `value` and the current state, it sets the given `category` in `current` to `value`.

    allSelected |> set Academic False |> isEventCategorySelected Academic --> False

    allSelected |> set Academic False |> isEventCategorySelected Work --> True

-}
set : EventCategory -> Bool -> SelectedEventCategories -> SelectedEventCategories
set category value current =
     case category of
        Academic -> if value then 
                        stringToEvent
                        <| String.fromList
                        <| replaceList 1 'C' 
                        <| String.toList 
                        <| eventToString current 
                    else
                        stringToEvent
                        <| String.fromList
                        <| replaceList 1 'N' 
                        <| String.toList 
                        <| eventToString current 
        Work -> if value then 
                    stringToEvent
                        <| String.fromList
                        <| replaceList 2 'W' 
                        <| String.toList 
                        <| eventToString current  
                else
                    stringToEvent
                        <| String.fromList
                        <| replaceList 2 'N' 
                        <| String.toList 
                        <| eventToString current   
        Project -> if value then 
                        stringToEvent
                        <| String.fromList
                        <| replaceList 3 'P' 
                        <| String.toList 
                        <| eventToString current  
                   else
                        stringToEvent
                        <| String.fromList
                        <| replaceList 3 'N' 
                        <| String.toList 
                        <| eventToString current  
        Award -> if value then 
                        stringToEvent
                        <| String.fromList
                        <| replaceList 4 'A' 
                        <| String.toList 
                        <| eventToString current  
                 else
                        stringToEvent
                        <| String.fromList
                        <| replaceList 4 'N' 
                        <| String.toList 
                        <| eventToString current 
    --Debug.todo "Implement Model.Event.Category.set"


eventToString : SelectedEventCategories -> String
eventToString events = 
    case events of 
        AcademicEventSelected -> "CNNN"
        WorkEventSelected -> "NWNN"
        ProjectEventSelected -> "NNPN"
        AwardEventSelected -> "NNNA"
        AllEventsSelected -> "CWPA"
        NoEventsSelected -> "NNNN"
        AcademicWorkEventSelected -> "CWNN"
        AcademicProjectEventSelected -> "CNPN"
        AcademicAwardEventSelected -> "CNNA"
        WorkProjectEventSelected -> "NWPN"
        WorkAwardEventSelected -> "NWNA"
        ProjectAwardEventSelected -> "NNPA"
        AcademicWorkProjectEventSelected -> "CWPN"
        AcademicWorkAwardEventSelected -> "CWNA"
        WorkProjectAwardEventSelected -> "NWPA"
        AcademicProjectAwardEventSelected -> "CNPA"

stringToEvent : String -> SelectedEventCategories
stringToEvent events = 
    case events of 
        "CNNN" -> AcademicEventSelected
        "NWNN" -> WorkEventSelected
        "NNPN" -> ProjectEventSelected
        "NNNA" -> AwardEventSelected
        "CWPA" -> AllEventsSelected
        "NNNN" -> NoEventsSelected
        "CWNN" -> AcademicWorkEventSelected
        "CNPN" -> AcademicProjectEventSelected
        "CNNA" -> AcademicAwardEventSelected
        "NWPN" -> WorkProjectEventSelected
        "NWNA" -> WorkAwardEventSelected
        "NNPA" -> ProjectAwardEventSelected
        "CWPN" -> AcademicWorkProjectEventSelected
        "CWNA" -> AcademicWorkAwardEventSelected
        "NWPA" -> WorkProjectAwardEventSelected
        "CNPA" -> AcademicProjectAwardEventSelected
        _ -> NoEventsSelected


replaceList : Int -> a -> List a -> List a
replaceList i ob list =
    let
        getPosition index l obj acc lst=
            case l of    
                [] -> lst
                x::xs -> if (acc == index) then 
                            getPosition index xs obj (acc + 1) (obj :: lst)
                        else 
                            getPosition index xs obj (acc + 1) (x :: lst)
    in 
        List.reverse (getPosition i list ob 1 [])


checkbox : String -> Bool -> EventCategory -> Html ( EventCategory, Bool )
checkbox name state category =
    div [ style "display" "inline", class "category-checkbox" ]
        [ input [ type_ "checkbox", onCheck (\c -> ( category, c )), checked state] []
        , text name
        ]


view : SelectedEventCategories -> Html ( EventCategory, Bool )
view model =
     div [] [
        checkbox "Academic Events" (isEventCategorySelected Academic model) Academic,
        checkbox "Work Events" (isEventCategorySelected Work model) Work,
        checkbox "Project Events" (isEventCategorySelected Project model) Project,
        checkbox "Award Events" (isEventCategorySelected Award model) Award
     ]
    --Debug.todo "Implement the Model.Event.Category.view function"
