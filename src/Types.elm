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
    , isFlipped : Bool
    , animation : Maybe Animation
    , id : Int
    }


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
    }


type Msg
    = Default
    | WinSize Win.Size
    | FlipCard Int
    | Shuffle Int
    | Tick Time
