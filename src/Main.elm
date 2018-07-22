port module Main exposing (..)

import Animation
    exposing
        ( Animation
        , animate
        , animation
        , duration
        , ease
        , from
        , getDuration
        , retarget
        , speed
        , to
        )
import AnimationFrame exposing (..)
import Color exposing (..)
import Dict exposing (..)
import Ease exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, program)
import Html.Attributes exposing (style)
import Random exposing (..)
import Task exposing (attempt)
import Time exposing (Time, millisecond, now)
import Types exposing (..)
import View exposing (..)
import Window as Win


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    { winSize = { height = 1024, width = 1920 }
    , seed = Random.initialSeed 0
    , deck = deck
    , playerDeck = []
    , opponentDeck = []
    , currentTick = 0
    , cardWidth = 334
    , cardHeight = 486
    }
        ! [ shuffleCmd
          , getWinSize
          ]


getWinSize =
    Task.perform WinSize Win.size


shuffleCmd =
    attempt
        (\t ->
            case t of
                Err _ ->
                    Shuffle 0

                Ok t2 ->
                    Shuffle (round t2)
        )
        now



--UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WinSize ({ width, height } as s) ->
            let
                newCardHeight =
                    round <| 0.8 * toFloat height / 3

                newCardWidth =
                    round <| toFloat newCardHeight / r
            in
            { model
                | winSize = s
                , cardHeight = toFloat newCardHeight
                , cardWidth = toFloat newCardWidth
            }
                ! []

        Shuffle t ->
            let
                shuffledDeck =
                    shuffle model.deck (Random.initialSeed t)
            in
            { model
                | deck = []
                , playerDeck = List.take 27 shuffledDeck
                , opponentDeck = List.drop 27 shuffledDeck
            }
                ! []

        FlipCard id ->
            let
                setAnim anim isFlipped =
                    case ( anim, isFlipped ) of
                        ( Nothing, True ) ->
                            Just
                                (animation (.currentTick model)
                                    |> from 180
                                    |> to 0
                                    |> duration (1000 * millisecond)
                                    |> ease Ease.outCubic
                                )

                        ( Nothing, False ) ->
                            Just
                                (animation (.currentTick model)
                                    |> from 0
                                    |> to 180
                                    |> duration (1000 * millisecond)
                                    |> ease Ease.outCubic
                                )

                        ( Just a, True ) ->
                            --Just a
                            Just
                                (retarget (.currentTick model) 0 a)

                        ( Just a, False ) ->
                            --Just a
                            Just
                                (retarget (.currentTick model) 180 a)

                flipCard xs =
                    List.foldr
                        (\c acc ->
                            if .id c == id then
                                { c
                                    | isFlipped = not (.isFlipped c)
                                    , animation =
                                        setAnim (.animation c)
                                            (.isFlipped c)
                                }
                                    :: acc
                            else
                                c :: acc
                        )
                        []
                        xs
            in
            { model
                | playerDeck = flipCard model.playerDeck
                , opponentDeck = flipCard model.opponentDeck
            }
                ! []

        Tick t ->
            { model | currentTick = t } ! []

        Default ->
            model ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Win.resizes WinSize
        , times Tick
        ]


type WarResult
    = Win
    | Lose
    | Draw


war : Card -> Card -> WarResult
war c1 c2 =
    if c1.value > c2.value then
        Win
    else if c1.value < c2.value then
        Lose
    else
        Draw


shuffle : List a -> Seed -> List a
shuffle xs seed =
    let
        l =
            List.length xs

        g =
            int 0 (10 * l)

        zip =
            List.map2 (,)

        indexesGen =
            Random.list l g

        indexList =
            Tuple.first (Random.step indexesGen seed)

        sortedList =
            List.sortWith (\( i1, _ ) ( i2, _ ) -> compare i1 i2) (zip indexList xs)
    in
    List.map Tuple.second sortedList


deck =
    let
        makeCard id ( suit, ( value, sprite ) ) =
            { suit = suit
            , value = value
            , sprite = sprite
            , isFlipped = True
            , animation = Nothing
            , id = id
            }
    in
    List.indexedMap makeCard
        [ ( Club, ( 1, ( 0, 1 ) ) )
        , ( Club, ( 2, ( 0, 2 ) ) )
        , ( Club, ( 3, ( 0, 3 ) ) )
        , ( Club, ( 4, ( 0, 4 ) ) )
        , ( Club, ( 5, ( 0, 5 ) ) )
        , ( Club, ( 6, ( 0, 6 ) ) )
        , ( Club, ( 7, ( 0, 7 ) ) )
        , ( Club, ( 8, ( 0, 8 ) ) )
        , ( Club, ( 9, ( 0, 9 ) ) )
        , ( Club, ( 10, ( 0, 10 ) ) )
        , ( Club, ( 11, ( 0, 11 ) ) )
        , ( Club, ( 12, ( 0, 12 ) ) )
        , ( Club, ( 13, ( 0, 0 ) ) )
        , ( Diamond, ( 1, ( 1, 1 ) ) )
        , ( Diamond, ( 2, ( 1, 2 ) ) )
        , ( Diamond, ( 3, ( 1, 3 ) ) )
        , ( Diamond, ( 4, ( 1, 4 ) ) )
        , ( Diamond, ( 5, ( 1, 5 ) ) )
        , ( Diamond, ( 6, ( 1, 6 ) ) )
        , ( Diamond, ( 7, ( 1, 7 ) ) )
        , ( Diamond, ( 8, ( 1, 8 ) ) )
        , ( Diamond, ( 9, ( 1, 9 ) ) )
        , ( Diamond, ( 10, ( 1, 10 ) ) )
        , ( Diamond, ( 11, ( 1, 11 ) ) )
        , ( Diamond, ( 12, ( 1, 12 ) ) )
        , ( Diamond, ( 13, ( 1, 0 ) ) )
        , ( Heart, ( 1, ( 2, 1 ) ) )
        , ( Heart, ( 2, ( 2, 2 ) ) )
        , ( Heart, ( 3, ( 2, 3 ) ) )
        , ( Heart, ( 4, ( 2, 4 ) ) )
        , ( Heart, ( 5, ( 2, 5 ) ) )
        , ( Heart, ( 6, ( 2, 6 ) ) )
        , ( Heart, ( 7, ( 2, 7 ) ) )
        , ( Heart, ( 8, ( 2, 8 ) ) )
        , ( Heart, ( 9, ( 2, 9 ) ) )
        , ( Heart, ( 10, ( 2, 10 ) ) )
        , ( Heart, ( 11, ( 2, 11 ) ) )
        , ( Heart, ( 12, ( 2, 12 ) ) )
        , ( Heart, ( 13, ( 2, 0 ) ) )
        , ( Spade, ( 1, ( 3, 1 ) ) )
        , ( Spade, ( 2, ( 3, 2 ) ) )
        , ( Spade, ( 3, ( 3, 3 ) ) )
        , ( Spade, ( 4, ( 3, 4 ) ) )
        , ( Spade, ( 5, ( 3, 5 ) ) )
        , ( Spade, ( 6, ( 3, 6 ) ) )
        , ( Spade, ( 7, ( 3, 7 ) ) )
        , ( Spade, ( 8, ( 3, 8 ) ) )
        , ( Spade, ( 9, ( 3, 9 ) ) )
        , ( Spade, ( 10, ( 3, 10 ) ) )
        , ( Spade, ( 11, ( 3, 11 ) ) )
        , ( Spade, ( 12, ( 3, 12 ) ) )
        , ( Spade, ( 13, ( 3, 0 ) ) )
        , ( RedJocker, ( 14, ( 4, 1 ) ) )
        , ( BlackJocker, ( 14, ( 4, 0 ) ) )
        ]
