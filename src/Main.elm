module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Point2D exposing (Point2D)
import Random exposing (Generator, Seed)
import Rigidbody exposing (Rigidbody)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Time exposing (Posix)


type alias Model =
    { agents : List Agent
    , randSeed : Seed
    }


type alias Agent =
    { rigidbody : Rigidbody
    }


init : ( Model, Cmd Msg )
init =
    ( { agents = []
      , randSeed = Random.initialSeed 42
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | CreateAgent
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CreateAgent ->
            let
                ( newAgent, newSeed ) =
                    createAgent model.randSeed
            in
            ( { model
                | agents =
                    if List.length model.agents < 30 then
                        newAgent :: model.agents

                    else
                        model.agents
                , randSeed = newSeed
              }
            , Cmd.none
            )

        Tick time ->
            ( { model | agents = updateAgents model.agents }, Cmd.none )


createAgent : Seed -> ( Agent, Seed )
createAgent seed =
    let
        ( ( xPos, yPos ), seed1 ) =
            Random.step (randomPoint 400) seed
    in
    ( { rigidbody =
            { position = Point2D (xPos + 400) (yPos + 400)
            , velocity = Point2D.origin
            , acceleration = Point2D.origin
            , maxSpeed = 1
            , maxForce = 1
            , mass = 1
            , forces = []
            }
      }
    , seed1
    )


updateAgents : List Agent -> List Agent
updateAgents agents =
    agents
        |> List.map (\agent -> { agent | rigidbody = Rigidbody.update agent.rigidbody })
        |> List.map (updateAgent agents)
        |> List.filter (.rigidbody >> .position >> isInBounds)


isInBounds : Point2D -> Bool
isInBounds point =
    point.x > 0 && point.x < 800 && point.y > 0 && point.y < 800


updateAgent : List Agent -> Agent -> Agent
updateAgent allAgents agent =
    let
        neighbors =
            List.filter
                (\other ->
                    Point2D.isNeighbor
                        200
                        agent.rigidbody.position
                        other.rigidbody.position
                )
                allAgents

        center =
            Point2D.average (List.map (.rigidbody >> .position) neighbors)

        averageHeading =
            Point2D.average (List.map (.rigidbody >> .velocity) neighbors)

        pushPull =
            if Point2D.distanceSq agent.rigidbody.position center > (50 ^ 2) then
                Point2D.sub center agent.rigidbody.position

            else
                Point2D.sub agent.rigidbody.position center
    in
    { agent
        | rigidbody =
            agent.rigidbody
                |> Rigidbody.addForce pushPull
                |> Rigidbody.addForce averageHeading
    }


view : Model -> Document Msg
view model =
    let
        center =
            Point2D.average (List.map (.position << .rigidbody) model.agents)
    in
    { title = "BioBlocks"
    , body =
        [ div []
            [ Svg.svg
                [ SvgAttr.width "800"
                , SvgAttr.height "800"
                , SvgAttr.viewBox "0 0 800 800"
                ]
                ([ Svg.rect
                    [ SvgAttr.x "0"
                    , SvgAttr.y "0"
                    , SvgAttr.width "800"
                    , SvgAttr.height "800"
                    , SvgAttr.fill "#222"
                    ]
                    []

                 --  , viewPoint center
                 ]
                    ++ List.concat
                        (List.map
                            (\agent ->
                                let
                                    nearestAgents =
                                        List.sortBy
                                            (\other ->
                                                Point2D.distanceSq
                                                    agent.rigidbody.position
                                                    other.rigidbody.position
                                            )
                                            model.agents
                                in
                                case nearestAgents of
                                    self :: other :: _ ->
                                        [ viewLine agent.rigidbody.position other.rigidbody.position ]

                                    _ ->
                                        []
                            )
                            model.agents
                        )
                    ++ List.map (viewRigidbody << .rigidbody) model.agents
                )
            ]
        ]
    }


viewPoint : Point2D -> Svg Msg
viewPoint { x, y } =
    Svg.circle
        [ SvgAttr.cx (String.fromInt <| floor x)
        , SvgAttr.cy (String.fromInt <| floor y)
        , SvgAttr.r "2"
        , SvgAttr.fill "#eee"
        ]
        []


viewLine : Point2D -> Point2D -> Svg Msg
viewLine p1 p2 =
    let
        floorString =
            String.fromInt << floor
    in
    Svg.line
        [ SvgAttr.x1 (floorString p1.x)
        , SvgAttr.y1 (floorString p1.y)
        , SvgAttr.x2 (floorString p2.x)
        , SvgAttr.y2 (floorString p2.y)
        , SvgAttr.stroke "#eee"
        , SvgAttr.strokeWidth "2"
        ]
        []


viewSquare : Float -> Point2D -> Svg Msg
viewSquare size center =
    let
        halfSize =
            size / 2
    in
    Svg.rect
        [ SvgAttr.x <| String.fromInt <| floor <| center.x - halfSize
        , SvgAttr.y <| String.fromInt <| floor <| center.y - halfSize
        , SvgAttr.width <| String.fromInt <| floor size
        , SvgAttr.height <| String.fromInt <| floor size
        , SvgAttr.fill "#eee"
        ]
        []


viewCircle : Float -> Point2D -> Svg Msg
viewCircle radius center =
    Svg.circle
        [ SvgAttr.cx <| String.fromInt <| floor <| center.x
        , SvgAttr.cy <| String.fromInt <| floor <| center.y
        , SvgAttr.r <| String.fromInt <| floor <| radius
        , SvgAttr.fill "#222"

        -- , SvgAttr.fillOpacity "0"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.stroke "#eee"
        ]
        []


viewRigidbody : Rigidbody -> Svg Msg
viewRigidbody rb =
    Svg.g []
        ([ -- viewSquare 16 rb.position
           viewCircle 6 rb.position

         --  , viewLine rb.position (Point2D.add rb.position (Point2D.mul rb.velocity 4))
         ]
         -- ++ List.map
         --     (\force ->
         --         viewLine rb.position
         --             (Point2D.add rb.position (Point2D.mul force 4))
         --     )
         --     rb.forces
        )


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrame (\time -> Tick time)
        , Time.every 200 (\_ -> CreateAgent)
        ]


randomPoint : Float -> Generator ( Float, Float )
randomPoint scalar =
    Random.pair (Random.float -scalar scalar) (Random.float -scalar scalar)
