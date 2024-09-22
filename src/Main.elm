module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Task =
    { id : Int
    , content : String
    , category : Category
    , destroyedAt : Time.Posix
    }


type Category
    = NotKnowing
    | Action
    | Done
    | Destroyed



categories : List Category
categories =
    [ NotKnowing, Action, Done, Destroyed ]


type alias Model =
    { tasks : List Task
    , nextId : Int
    , newTaskContent : String
    , currentTime : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tasks = []
      , nextId = 1
      , newTaskContent = ""
      , currentTime = Time.millisToPosix 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AddTask
    | UpdateNewTaskContent String
    | MoveTask Int Category
    | Tick Time.Posix

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTask ->
            if model.newTaskContent == "" then
                ( model, Cmd.none )

            else
                ( { model
                    | tasks = (Task model.nextId model.newTaskContent NotKnowing (getDestroyedAt model.currentTime)) :: model.tasks
                    , nextId = model.nextId + 1
                    , newTaskContent = ""
                  }
                , Cmd.none
                )

        UpdateNewTaskContent content ->
            ( { model | newTaskContent = content }
            , Cmd.none
            )

        MoveTask id category ->
            let
                movedTasks : List Task
                movedTasks = List.map (moveTaskIfMatch id category) model.tasks
                notKnowingOrActionList : List Task
                notKnowingOrActionList =  List.filter (\t -> (t.category == NotKnowing || t.category==Action)) movedTasks
                doneList: List Task
                doneList = List.filter (\t -> t.category == Done) movedTasks
                destroyedList: List Task
                destroyedList = List.filter (\t -> t.category == Destroyed) movedTasks
                updatedTaskList = notKnowingOrActionList ++ (doneList ++ destroyedList)
            in
                ( { model | tasks = List.map (moveTaskIfMatch id category) updatedTaskList }
                , Cmd.none
                )

        Tick newTime ->
            ( { model
                | currentTime = newTime
                , tasks = List.map (expireTaskIfNeeded newTime) model.tasks
              }
            , Cmd.none
            )


moveTaskIfMatch : Int -> Category -> Task -> Task
moveTaskIfMatch id category task =
    if task.id == id then
        { task | category = category }

    else
        task


expireTaskIfNeeded : Time.Posix -> Task -> Task
expireTaskIfNeeded currentTime task =
    if (Time.posixToMillis currentTime > Time.posixToMillis task.destroyedAt) && task.category /= Done then
        { task | category = Destroyed }

    else
        task



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Task Management" ]
        , viewTaskInput model
        , text (Debug.toString model)
        , viewTaskTable model
        ]

viewTaskInput : Model -> Html Msg
viewTaskInput model = div [] [ input [ type_ "text", placeholder "New task", value model.newTaskContent, onInput UpdateNewTaskContent ] []
        , button [ onClick AddTask ] [ text "Add Task" ]
        ]


viewTask : Time.Posix -> Category -> Task -> Html Msg
viewTask currentTime currentCategory task =
    li [ style "border" "1px solid black" ]
        [ text task.content
        , div [] (viewTaskButtons currentCategory task)
        , div [] [text <| computeTimeLeft currentTime task.destroyedAt]
        ]

millisecondsInDay = (24 * 60 * 60 * 1000)
millisecondsInHour = (60 * 60 * 1000)
millisecondsInMinute = (60*1000)
computeTimeLeft :  Time.Posix -> Time.Posix -> String
computeTimeLeft currentTick destroyTime =
        let
            destroyTimeMillis = Time.posixToMillis destroyTime
            currentTimeMillis = Time.posixToMillis  currentTick
            timeLeft = destroyTimeMillis - currentTimeMillis
            days = timeLeft  // millisecondsInDay
            hoursLeft = timeLeft -  (days * millisecondsInDay)
            hours = hoursLeft // millisecondsInHour
            minutesLeft = hoursLeft - (hours*millisecondsInHour)
            minutes = minutesLeft // millisecondsInMinute
            secondsLeft = minutesLeft - (minutes * millisecondsInMinute)
            seconds = secondsLeft // (1000)


        in
            (
            "Expires in "
            ++ String.fromInt days
            ++ ":"
            ++ String.fromInt hours
            ++ ":"
            ++ String.fromInt minutes
             ++ ":"
            ++ String.fromInt seconds
            )

viewTaskButtons : Category -> Task -> List (Html Msg)
viewTaskButtons currentCategory task =
    case task.category of
        Action ->
            List.filter (isNotCurrentCategory currentCategory) [ Done, Destroyed ]
                |> List.map (createMoveButton task.id)

        NotKnowing ->
            List.map (createMoveButton task.id) [ Action, Done, Destroyed ]

        _ ->
            []


isNotCurrentCategory : Category -> Category -> Bool
isNotCurrentCategory currentCategory category =
    currentCategory /= category


createMoveButton : Int -> Category -> Html Msg
createMoveButton taskId category =
    button [ onClick (MoveTask taskId category) ] [ text (categoryToString category) ]


categoryToString : Category -> String
categoryToString category =
    case category of
        NotKnowing ->
            "Not Knowing"

        Action ->
            "Action"

        Done ->
            "Done"

        Destroyed ->
            "Destroyed"


viewTH : Category -> Html Msg
viewTH category =
    th [] [ text (categoryToString category) ]


viewTaskTable :Model -> Html Msg
viewTaskTable model =
    table [ style "width" "100%", style "border-collapse" "collapse" ]
        [ thead []
            [ tr [] <| List.map viewTH categories ]
            , tbody [] (List.map (viewTaskRow model.currentTime) model.tasks)
            ]


viewTaskRow :Time.Posix -> Task -> Html Msg
viewTaskRow currentTime task =

    tr []
        [
          viewCategoryCell currentTime NotKnowing task
        , viewCategoryCell currentTime Action task
        , viewCategoryCell currentTime Done task
        , viewCategoryCell currentTime Destroyed task
        ]

viewCategoryCell : Time.Posix-> Category -> Task -> Html Msg
viewCategoryCell currentTime category task =
    td
         [ style "text-align" "center"
        , style "background-color"
            (if task.category == category then
                "#e0e0e0"

             else
                "white"
            )
        ]
        [
            if (task.category == category) then
            viewTask currentTime category task
          else
            text ""
        ]
getDestroyedAt : Time.Posix -> Time.Posix
getDestroyedAt currentTime = Time.millisToPosix <| (Time.posixToMillis currentTime) + (14 * millisecondsInDay)