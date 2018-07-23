module AnimationHelper exposing (..)

import Animation
    exposing
        ( animate
        , animation
        , delay
        , duration
        , ease
        , from
        , getDuration
        , isDone
        , retarget
        , speed
        , to
        )
import Ease exposing (..)
import Task exposing (perform, succeed)
import Time exposing (Time, millisecond)
import Types exposing (..)


flipCard : Time -> Time -> CardId -> List AnimationMeta
flipCard ct del id =
    [ { animation =
            animation ct
                |> from 0
                |> to 90
                |> duration (250 * millisecond)
                |> ease Ease.inCubic
                |> delay del
      , property = Flip
      , msg = Just <| FlipCard id
      }
    , { animation =
            animation ct
                |> from 0
                |> to 90
                |> duration (250 * millisecond)
                |> ease Ease.outCubic
                |> delay (del + 250 * millisecond)
      , property = Flip
      , msg = Nothing
      }

    --, { animation =
    --        animation ct
    --            |> from 0
    --            |> to 90
    --            |> duration (250 * millisecond)
    --            |> ease Ease.outCubic
    --            |> delay del
    --  , property = Flip
    --  , msg = Just <| FlipCard id
    --  }
    ]


moveX : Time -> Time -> Float -> CardId -> List AnimationMeta
moveX ct del offset id =
    [ { animation =
            animation ct
                |> from 0
                |> to offset
                |> duration (500 * millisecond)
                |> ease Ease.outCubic
                |> delay del
      , property = MoveX
      , msg = Nothing
      }
    ]


moveY : Time -> Time -> Float -> CardId -> List AnimationMeta
moveY ct del offset id =
    [ { animation =
            animation ct
                |> from 0
                |> to offset
                |> duration (500 * millisecond)
                |> ease Ease.outCubic
                |> delay del
      , property = MoveY
      , msg = Nothing
      }
    ]


flipBack : Time -> Time -> CardId -> AnimationMeta
flipBack ct del id =
    { animation =
        animation ct
            |> from 0
            |> to 180
            |> duration (500 * millisecond)
            |> ease Ease.outCubic
            |> delay del
    , property = Flip
    , msg = Just <| FlipCard id
    }



--updateCardsAnimations : Time -> List Card -> ( List Card, List (Cmd Msg) )
--updateCardsAnimations currentTime cards =
--    List.foldr
--        (\c ( newCards, cmds ) ->
--            case updateAnimations currentTime c.animations of
--                ( anims, Nothing ) ->
--                    ( { c | animations = anims } :: newCards, cmds )
--                ( anims, Just msg ) ->
--                    ( { c | animations = anims } :: newCards, toCmd msg :: cmds )
--        )
--        ( [], [] )
--        cards
--updateAnimations : Time -> List AnimationMeta -> ( List AnimationMeta, Maybe Msg )
--updateAnimations currentTime anims =
--    case anims of
--        [] ->
--            ( [], Nothing )
--        ({ animation, msg } as anim) :: xs ->
--            if isDone currentTime animation then
--                ( xs
--                , msg
--                )
--            else
--                ( anim :: xs, Nothing )


updateCardsAnimations : Time -> List Card -> ( List Card, List (Cmd Msg) )
updateCardsAnimations currentTime cards =
    List.foldr
        (\c ( newCards, cmds ) ->
            case updateAnimations currentTime c.animations of
                ( anims, [] ) ->
                    ( { c | animations = anims } :: newCards, cmds )

                ( anims, newCmds ) ->
                    ( { c | animations = anims } :: newCards, newCmds ++ cmds )
        )
        ( [], [] )
        cards


updateAnimations : Time -> List AnimationMeta -> ( List AnimationMeta, List (Cmd Msg) )
updateAnimations currentTime anims =
    let
        update ({ animation, msg } as anim) ( anims, cmds ) =
            if isDone currentTime animation then
                case msg of
                    Nothing ->
                        ( anim :: anims, cmds )

                    Just msg ->
                        ( { anim | msg = Nothing } :: anims, toCmd msg :: cmds )
            else
                ( anim :: anims, cmds )
    in
    List.foldr update ( [], [] ) anims



--|> Tuple.mapFirst List.reverse


toCmd c =
    Task.perform (\_ -> c) (Task.succeed "")
