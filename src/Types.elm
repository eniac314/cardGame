module Types exposing (..)

import Animation
    exposing
        ( Animation
        )
import Random exposing (Seed)
import Time exposing (Time)
import Window as Win


type alias Card =
    { suit : Suit
    , value : Int
    , sprite : ( Float, Float )

    --, xOffset : Int
    --, yOffset : Int
    --, angle : Int
    , isFlipped : Bool
    , animations : List AnimationMeta
    , id : CardId
    }


type alias CardId =
    Int


type alias AnimationMeta =
    { animation : Animation
    , property : AnimProperty
    , msg : Maybe Msg
    }


type AnimProperty
    = Flip
    | MoveX
    | MoveY


type Suit
    = Club
    | Diamond
    | Heart
    | Spade
    | BlackJocker
    | RedJocker


type alias Model =
    { winSize : Win.Size
    , seed : Random.Seed
    , deck : List Card
    , playerDeck : List Card
    , opponentDeck : List Card
    , currentTick : Time
    , cardWidth : Float
    , cardHeight : Float
    , tmp : Int
    }


type Msg
    = Default
    | WinSize Win.Size
    | FlipCard Int
    | Animate CardId
    | Shuffle Int
    | Tick Time
    | UpdateAnimations Time
