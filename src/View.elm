module View exposing (..)

import Animation
    exposing
        ( animate
        , isScheduled
        )
import Color exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import Time exposing (Time)
import Types exposing (..)


--w =
--    175


r =
    486 / 334



--h =
--    round (w * r)


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.tiled "/assets/bg.gif"

        --, htmlAttribute <|
        --    style [ ( "perspective", "600px" ) ]
        --, Background.tiled
        --, centerX
        ]
    <|
        column
            [ width fill
            , height fill

            --, Background.color Color.blue
            ]
            [ opponentPanelView model
            , centralPanelView model
            , playerPanelView model

            --, handView model.currentTick (List.take 5 model.deck)
            ]


opponentPanelView model =
    Element.row
        [ width (px <| model.winSize.width // 2)
        , height fill
        , centerX
        , spacing 100

        --, Background.color (Color.rgba 0 255 0 0.5)
        ]
        [ el [ width (px (round model.cardWidth)) ] Element.none
        , handView model.tmp model.currentTick ( model.cardWidth, model.cardHeight ) -1 model.opponentDeck
        ]


centralPanelView model =
    Element.row
        [ width (px <| model.winSize.width // 2)
        , height fill
        , centerX
        , spacing 100

        --, Background.color (Color.rgba 255 0 0 0.5)
        ]
        [ el
            [ width (px (round model.cardWidth))
            , height (px (round model.cardHeight))
            ]
            Element.none
        , el
            [ width (px (round model.cardWidth))
            , height (px (round model.cardHeight))
            ]
            Element.none
        ]


playerPanelView model =
    Element.row
        [ width (px <| model.winSize.width // 2)
        , height fill
        , centerX
        , spacing 100

        --, Background.color (Color.rgba 0 0 255 0.5)
        ]
        [ handView model.tmp model.currentTick ( model.cardWidth, model.cardHeight ) 1 model.playerDeck
        , el [ width (px (round model.cardWidth)) ] Element.none
        ]


handView : Int -> Time -> ( Float, Float ) -> Int -> List Card -> Element Msg
handView tmp currentTick ( w, h ) orientation cards =
    row
        [ spacing (round -w + (2 * orientation))
        , width shrink
        , centerX
        , centerY

        --, htmlAttribute <|
        --    style [ ( "perspective", "600px" ) ]
        --, Background.color Color.red
        ]
        (List.map
            (cardView currentTick
                ( w, h )
                orientation
                -- tmp
                (List.length cards)
            )
            cards
        )


cardView : Time -> ( Float, Float ) -> Int -> Int -> Card -> Element Msg
cardView currentTick ( w, h ) orientation nbrCards { sprite, isFlipped, animations, id } =
    let
        xOffset =
            Tuple.second sprite * -1 * w

        yOffset =
            Tuple.first sprite * -1 * w * r

        spritePos =
            if isFlipped then
                toString (-2 * w) ++ "px " ++ toString (-4 * w * r) ++ "px"
            else
                toString xOffset ++ "px " ++ toString yOffset ++ "px"

        transform =
            List.foldr
                (\anim acc ->
                    case anim.property of
                        MoveX ->
                            --acc
                            acc ++ "translateX(" ++ toString (toFloat orientation * animate currentTick anim.animation) ++ "px) "

                        MoveY ->
                            --acc
                            acc ++ "translateY(" ++ toString (toFloat orientation * animate currentTick anim.animation) ++ "px) "

                        Flip ->
                            acc ++ "rotateY(" ++ toString (animate currentTick anim.animation) ++ "deg) "
                 --++ acc
                )
                ""
                animations

        transformOrigin =
            toString (orientation * (100 + round w) + round w // 2)
                ++ "px "
                ++ toString (orientation * -300 - round h // 2)
                ++ "px"
    in
    --el
    --    [ padding 5
    --    , Background.color Color.white
    --    , Border.width 1
    --    , Border.rounded 5
    --    ]
    el
        [ width (px <| round w)
        , height (px <| round h)

        --, Border.glow Color.black 15
        --, Border.shadow
        --    { offset = ( 2, 2 )
        --    , blur = 4
        --    , size = 2
        --    , color = Color.black
        --    }
        , Border.width 1
        , Border.rounded 5
        , padding 5
        , Events.onClick (Animate id)
        , htmlAttribute <|
            style
                [ ( "background-image", "url(\"/assets/playing cards Merge.jpg\")" )
                , ( "background-position", spritePos )
                , ( "background-size", toString (w * 13) ++ "px " ++ toString (h * 5) ++ "px" )

                --, ( "transform", "translateX(500px)" )
                , ( "perspective", "600px" )
                , ( "transform-origin", transformOrigin )
                , ( "transform", transform )
                ]
        ]
        none
