port module Main exposing (main, taskDecoder)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, dict)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Markdown
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
    { id : String
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


type alias NotKnowingTasks =
    Dict String Task


type alias ActionTasks =
    Dict String Task


type alias DoneTasks =
    Dict String Task


type alias DestroyedTasks =
    Dict String Task


type alias Model =
    { notKnowingTasks : NotKnowingTasks
    , actionTasks : ActionTasks
    , doneTasks : DoneTasks
    , destroyedTasks : DestroyedTasks
    , newTaskContent : String
    , currentTime : Time.Posix
    }


type alias LoadModel =
    { notKnowingTasks : NotKnowingTasks
    , actionTasks : ActionTasks
    , doneTasks : DoneTasks
    , destroyedTasks : DestroyedTasks
    }


type alias SaveModel =
    { notKnowingTasks : NotKnowingTasks
    , actionTasks : ActionTasks
    , doneTasks : DoneTasks
    , destroyedTasks : DestroyedTasks
    }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue loadModelDecoder flags of
        Ok loadModel ->
            ( { notKnowingTasks = loadModel.notKnowingTasks
              , actionTasks = loadModel.actionTasks
              , doneTasks = loadModel.doneTasks
              , destroyedTasks = loadModel.destroyedTasks
              , newTaskContent = ""
              , currentTime = Time.millisToPosix 0
              }
            , Cmd.none
            )

        Err e ->
            Debug.log (Decode.errorToString e)
            ( { notKnowingTasks = Dict.empty
              , actionTasks = Dict.empty
              , doneTasks = Dict.empty
              , destroyedTasks = Dict.empty
              , newTaskContent = ""
              , currentTime = Time.millisToPosix 0
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = AddTask
    | UpdateNewTaskContent String
    | MoveTask String Category Category
    | Tick Time.Posix
    | LoadTasksFromLocalStorage Json.Encode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTask ->
            if model.newTaskContent == "" then
                ( model, Cmd.none )

            else
                let
                    id =
                        String.fromInt (Time.posixToMillis model.currentTime)

                    newNotKnowingTasks =
                        Dict.insert id (Task id model.newTaskContent NotKnowing (getDestroyedAt model.currentTime)) model.notKnowingTasks
                in
                ( { model
                    | notKnowingTasks = newNotKnowingTasks
                    , newTaskContent = ""
                  }
                , saveTasks <|
                    encodeTasks
                        { notKnowingTasks = newNotKnowingTasks
                        , actionTasks = model.actionTasks
                        , doneTasks = model.doneTasks
                        , destroyedTasks = model.destroyedTasks
                        }
                )

        UpdateNewTaskContent content ->
            ( { model | newTaskContent = content }
            , Cmd.none
            )

        Tick newTime ->
            let
                expiringActionTasks =
                    Dict.filter (\_ task -> Time.posixToMillis newTime > Time.posixToMillis task.destroyedAt) model.actionTasks

                expiringNotKnowingTasks =
                    Dict.filter (\_ task -> Time.posixToMillis newTime > Time.posixToMillis task.destroyedAt) model.notKnowingTasks

                allExpiringTasks =
                    Dict.union expiringActionTasks expiringNotKnowingTasks

                newModel =
                    case Dict.toList allExpiringTasks of
                        ( taskId, task ) :: _ ->
                            let
                                tasks =
                                    moveTasks model taskId task.category Destroyed
                            in
                            { model
                                | currentTime = newTime
                                , actionTasks = tasks.actionTasks
                                , notKnowingTasks = tasks.notKnowingTasks
                                , doneTasks = tasks.doneTasks
                                , destroyedTasks = tasks.destroyedTasks
                            }

                        [] ->
                            { model | currentTime = newTime }
            in
            ( newModel
            , Cmd.none
            )

        LoadTasksFromLocalStorage json ->
            case Decode.decodeValue loadModelDecoder json of
                Ok loadedTasks ->
                    ( { model
                        | actionTasks = loadedTasks.actionTasks
                        , notKnowingTasks = loadedTasks.notKnowingTasks
                        , doneTasks = loadedTasks.doneTasks
                        , destroyedTasks = loadedTasks.destroyedTasks
                      }
                    , Cmd.none
                    )

                Err x ->
                    ( model, Cmd.none )

        MoveTask taskId fromCategory toCategory ->
            let
                tasks =
                    moveTasks model taskId fromCategory toCategory
            in
            ( { model
                | notKnowingTasks = tasks.notKnowingTasks
                , actionTasks = tasks.actionTasks
                , doneTasks = tasks.doneTasks
                , destroyedTasks = tasks.destroyedTasks
              }
            , saveTasks <|
                encodeTasks
                    { notKnowingTasks = tasks.notKnowingTasks
                    , actionTasks = tasks.actionTasks
                    , doneTasks = tasks.doneTasks
                    , destroyedTasks = tasks.destroyedTasks
                    }
            )


moveTasks : Model -> String -> Category -> Category -> SaveModel
moveTasks model taskId fromCategory toCategory =
    let
        moveTask : Dict String Task -> Dict String Task -> ( Dict String Task, Dict String Task )
        moveTask fromDict toDict =
            case Dict.get taskId fromDict of
                Just task ->
                    ( Dict.remove taskId fromDict
                    , Dict.insert taskId { task | category = toCategory } toDict
                    )

                Nothing ->
                    ( fromDict, toDict )

        ( newFromDict, newToDict ) =
            case ( fromCategory, toCategory ) of
                ( NotKnowing, Action ) ->
                    moveTask model.notKnowingTasks model.actionTasks

                ( NotKnowing, Done ) ->
                    moveTask model.notKnowingTasks model.doneTasks

                ( NotKnowing, Destroyed ) ->
                    moveTask model.notKnowingTasks model.destroyedTasks

                ( Action, Done ) ->
                    moveTask model.actionTasks model.doneTasks

                ( Action, Destroyed ) ->
                    moveTask model.actionTasks model.destroyedTasks

                _ ->
                    ( Dict.empty, Dict.empty )

        notKnowingTasks =
            if fromCategory == NotKnowing then
                newFromDict

            else
                model.notKnowingTasks

        actionTasks =
            if fromCategory == Action then
                newFromDict

            else if toCategory == Action then
                newToDict

            else
                model.actionTasks

        doneTasks =
            if fromCategory == Done then
                newFromDict

            else if toCategory == Done then
                newToDict

            else
                model.doneTasks

        destroyedTasks =
            if toCategory == Destroyed then
                newToDict

            else
                model.destroyedTasks
    in
    SaveModel notKnowingTasks actionTasks doneTasks destroyedTasks



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.currentTime == Time.millisToPosix 0 then
        Sub.batch
            [ Time.every 1 Tick
            , loadTasks LoadTasksFromLocalStorage
            ]

    else
        Sub.batch
            [ Time.every 1000 Tick
            , loadTasks LoadTasksFromLocalStorage
            ]



-- VIEW


view : Model -> Html Msg
view model =
    div anonymousProBold
        [ h1 [] [ text "Done" ]
        , Html.main_ []
            [ viewTaskInput model
            , viewTaskTable model
            ]
        ]


viewTaskInput : Model -> Html Msg
viewTaskInput model =
    div []
        [ textarea [ Html.Attributes.placeholder "New task", Html.Attributes.value model.newTaskContent, onInput UpdateNewTaskContent ] []
        , button [ onClick AddTask ] [ text "Add Task" ]
        ]


viewExpiration : Time.Posix -> Task -> Maybe (Html Msg)
viewExpiration currentTime task =
    case task.category of
        NotKnowing ->
            Just <| text <| computeTimeLeft currentTime task.destroyedAt

        Action ->
            Just <| text <| computeTimeLeft currentTime task.destroyedAt

        _ ->
            Nothing


viewTask : Time.Posix -> Task -> Html Msg
viewTask currentTime task =
    let
        msgs =
            [ div (anonymousProRegular )
             [(Markdown.toHtml [Html.Attributes.style "display" "inline"] task.content)]

            , div [] (viewTaskButtons task)
            ]

        expirationMsg =
            case viewExpiration currentTime task of
                Just a ->
                    msgs ++ [ a ]

                Nothing ->
                    msgs
    in
    div [] expirationMsg


computeTimeLeft : Time.Posix -> Time.Posix -> String
computeTimeLeft currentTick destroyTime =
    let
        destroyTimeMillis =
            Time.posixToMillis destroyTime

        currentTimeMillis =
            Time.posixToMillis currentTick

        timeLeft =
            destroyTimeMillis - currentTimeMillis

        days =
            timeLeft // millisecondsInDay

        hoursLeft =
            timeLeft - (days * millisecondsInDay)

        hours =
            hoursLeft // millisecondsInHour

        minutesLeft =
            hoursLeft - (hours * millisecondsInHour)

        minutes =
            minutesLeft // millisecondsInMinute

        secondsLeft =
            minutesLeft - (minutes * millisecondsInMinute)

        seconds =
            secondsLeft // 1000
    in
    "Expires in "
        ++ String.fromInt days
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt hours)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt minutes)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt seconds)


viewTaskButtons : Task -> List (Html Msg)
viewTaskButtons task =
    case task.category of
        Action ->
            List.map (createMoveButton task task.category) [ Done, Destroyed ]

        NotKnowing ->
            List.map (createMoveButton task task.category) [ Action, Done, Destroyed ]

        _ ->
            []


createMoveButton : Task -> Category -> Category -> Html Msg
createMoveButton task oldCategory newCategory =
    let
        attrs =
            [ onClick (MoveTask task.id oldCategory newCategory) ] ++ moveTaskButtonAttributes newCategory ++ anonymousProRegularButton
    in
        button attrs [ text (categoryToString newCategory) ]


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
            "Destroy"


viewTH : Category -> Html Msg
viewTH category =
    th
        [ Html.Attributes.style "width" "25%" -- Set each column to 25% width
        , Html.Attributes.style "padding" "10px"
        , Html.Attributes.style "box-sizing" "border-box"
        ]
        [ h1 [] [ text (categoryToString category) ] ]


viewTaskTable : Model -> Html Msg
viewTaskTable model =
    table
        ([ Html.Attributes.style "width" "100%"
          , Html.Attributes.style "border-collapse" "separate"  -- Change this from "collapse" to "separate"
         , Html.Attributes.style "border-spacing" "10px"  -- Add this line for spacing between cells
         , Html.Attributes.style "table-layout" "fixed" -- Add this line to enforce fixed layout
         ]
            ++ anonymousProRegular
        )
        [ thead []
            [ tr [] <| List.map viewTH categories ]
        , tbody [] <|
            List.map (viewTaskRow model.currentTime) <|
                List.concat
                    [ Dict.values model.actionTasks
                    , Dict.values model.notKnowingTasks
                    , Dict.values model.doneTasks
                    , Dict.values model.destroyedTasks
                    ]
        ]


viewTaskRow : Time.Posix -> Task -> Html Msg
viewTaskRow currentTime task =
    tr []
        [ viewCategoryCell currentTime NotKnowing task
        , viewCategoryCell currentTime Action task
        , viewCategoryCell currentTime Done task
        , viewCategoryCell currentTime Destroyed task
        ]


moveTaskButtonAttributes : Category -> List (Attribute msg)
moveTaskButtonAttributes category =
    let
        backgroundColor =
            case category of
                NotKnowing ->
                    beigeStyle

                Action ->
                    greenStyle

                Done ->
                    goldStyle

                Destroyed ->
                    blueStyle

        fontColort =
            case category of
                NotKnowing ->
                    "#000000"

                Action ->
                    "#ffffff"

                Done ->
                    "#000000"

                Destroyed ->
                    whiteStyle
    in
    [ Html.Attributes.style "text-align" "center"
    , Html.Attributes.style "background-color" backgroundColor
    , Html.Attributes.style "color" fontColort
    ]


cellAttributes : Task -> Category -> List (Attribute msg)
cellAttributes task category =
    [ Html.Attributes.style "text-align" "center"
    , Html.Attributes.style "background-color"
        (if task.category == category then
            case category of
                NotKnowing ->
                    beigeStyle

                Action ->
                    greenStyle

                Done ->
                    goldStyle

                Destroyed ->
                    blueStyle

         else
            "white"
        )
    , Html.Attributes.style "color"
        (if task.category == category then
            case category of
                NotKnowing ->
                    "#000000"

                Action ->
                    "#ffffff"

                Done ->
                    "#000000"

                Destroyed ->
                    whiteStyle

         else
            "#000000"
        )
       , Html.Attributes.style "20px solid"
       (if task.category == category then
            case category of
                NotKnowing ->
                      "#000000" --beigeBorderStyle
                Action ->
                    greenStyle

                Done ->
                    goldStyle

                Destroyed ->
                    blueStyle
        else
            "#000000"
       )
       ]


viewCategoryCell : Time.Posix -> Category -> Task -> Html Msg
viewCategoryCell currentTime category task =
    td
        (cellAttributes task category ++
         [ Html.Attributes.style "padding" "10px"  -- Add padding inside cells
         , Html.Attributes.style "border-radius" "5px"  -- Optional: add rounded corners
         ])
        [ if task.category == category then
            viewTask currentTime task

          else
            text ""
        ]


getDestroyedAt : Time.Posix -> Time.Posix
getDestroyedAt currentTime =
    Time.millisToPosix <| Time.posixToMillis currentTime + (14 * millisecondsInDay)



-- PORTS


port loadTasks : (Json.Encode.Value -> msg) -> Sub msg


port sendLoadTasksMsg : () -> Cmd msg


port saveTasks : Json.Encode.Value -> Cmd msg


categoryDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Not Knowing" ->
                        Decode.succeed NotKnowing

                    "Action" ->
                        Decode.succeed Action

                    "Done" ->
                        Decode.succeed Done

                    "Destroyed" ->
                        Decode.succeed Destroyed

                    _ ->
                        Decode.fail "Invalid Category"
            )


encodeTask : Task -> Json.Encode.Value
encodeTask task =
    Json.Encode.object
        [ ( "id", Json.Encode.string task.id )
        , ( "content", Json.Encode.string task.content )
        , ( "category", Json.Encode.string <| categoryToString task.category )
        , ( "destroyedAt", Json.Encode.int <| Time.posixToMillis task.destroyedAt )
        ]


encodeTasks : SaveModel -> Json.Encode.Value
encodeTasks saveModel =
    Json.Encode.object
        [ ( "notKnowingTasks", Json.Encode.dict identity encodeTask saveModel.notKnowingTasks )
        , ( "actionTasks", Json.Encode.dict identity encodeTask saveModel.actionTasks )
        , ( "doneTasks", Json.Encode.dict identity encodeTask saveModel.doneTasks )
        , ( "destroyedTasks", Json.Encode.dict identity encodeTask saveModel.destroyedTasks )
        ]


posixDecoder =
    Decode.int
        |> Decode.andThen
            (\posix -> Decode.succeed (Time.millisToPosix posix))


dictTaskDecoder : Decoder (Dict String Task)
dictTaskDecoder =
    dict taskDecoder


taskDecoder : Decode.Decoder Task
taskDecoder =
    Decode.succeed Task
        |> required "id" Decode.string
        |> required "content" Decode.string
        |> required "category" categoryDecoder
        |> required "destroyedAt" posixDecoder


loadModelDecoder : Decode.Decoder LoadModel
loadModelDecoder =
    Decode.succeed LoadModel
        |> required "notKnowingTasks" dictTaskDecoder
        |> required "actionTasks" dictTaskDecoder
        |> required "doneTasks" dictTaskDecoder
        |> required "destroyedTasks" dictTaskDecoder


millisecondsInDay =
    24 * 60 * 60 * 1000


millisecondsInHour =
    60 * 60 * 1000


millisecondsInMinute =
    60 * 1000


anonymousProRegular : List (Html.Attribute msg)
anonymousProRegular =
    [ Html.Attributes.style "font-family" "\"Anonymous Pro\", monospace"
    , Html.Attributes.style "font-weight" "400"
    , Html.Attributes.style "font-style" "normal"
    ]


anonymousProRegularButton : List (Html.Attribute msg)
anonymousProRegularButton =
    [ Html.Attributes.style "font-family" "\"Anonymous Pro\", monospace"
    , Html.Attributes.style "font-weight" "400"
    , Html.Attributes.style "font-style" "normal"
    ]


anonymousProBold : List (Html.Attribute msg)
anonymousProBold =
    [ Html.Attributes.style "font-family" "\"Anonymous Pro\", monospace"
    , Html.Attributes.style "font-weight" "700"
    , Html.Attributes.style "font-style" "normal"
    ]


anonymousProRegularItalic : List (Html.Attribute msg)
anonymousProRegularItalic =
    [ Html.Attributes.style "font-family" "\"Anonymous Pro\", monospace"
    , Html.Attributes.style "font-weight" "400"
    , Html.Attributes.style "font-style" "italic"
    ]


anonymousProBoldItalic : List (Html.Attribute msg)
anonymousProBoldItalic =
    [ Html.Attributes.style "font-family" "\"Anonymous Pro\", monospace"
    , Html.Attributes.style "font-weight" "700"
    , Html.Attributes.style "font-style" "italic"
    ]

blueStyle =
    "#302EEC"

beigeStyle =
    "#fff6e4"

beigeBorderStyle =
    "#8A8161FF"

goldStyle =
    "#FFDE60"


greenStyle =
    "#28a745"


redStyle =
    "linear-gradient(to bottom, #ff4536 0%, #eb4033 100%)"


whiteStyle =
    "white"
