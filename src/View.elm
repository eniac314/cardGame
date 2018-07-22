module View exposing (..)

import Animation
    exposing
        ( animate
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
        , spacing 50
        , Background.color (Color.rgba 0 255 0 0.5)
        ]
        [ el [ width (px (round model.cardWidth)) ] Element.none
        , handView model.currentTick ( model.cardWidth, model.cardHeight ) model.opponentDeck
        ]


centralPanelView model =
    Element.row
        [ width (px <| model.winSize.width // 2)
        , height fill
        , centerX
        , spacing 50
        , Background.color (Color.rgba 255 0 0 0.5)
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
        , spacing 50
        , Background.color (Color.rgba 0 0 255 0.5)
        ]
        [ handView model.currentTick ( model.cardWidth, model.cardHeight ) model.playerDeck
        , el [ width (px (round model.cardWidth)) ] Element.none
        ]


handView : Time -> ( Float, Float ) -> List Card -> Element Msg
handView currentTick ( w, h ) cards =
    row
        [ spacing (round -w + 2)
        , width shrink
        , centerX
        , centerY
        , htmlAttribute <|
            style [ ( "perspective", "600px" ) ]

        --, Background.color Color.red
        ]
        (List.map (cardView currentTick ( w, h )) cards)


cardView : Time -> ( Float, Float ) -> Card -> Element Msg
cardView currentTick ( w, h ) { sprite, isFlipped, animation, id } =
    let
        xOffset =
            Tuple.second sprite * -1 * w

        yOffset =
            Tuple.first sprite * -1 * w * r

        spritePos =
            case animation of
                Nothing ->
                    if isFlipped then
                        toString (-2 * w) ++ "px " ++ toString (-4 * w * r) ++ "px"
                    else
                        toString xOffset ++ "px " ++ toString yOffset ++ "px"

                Just a ->
                    if animate currentTick a > 90 then
                        toString (-2 * w) ++ "px " ++ toString (-4 * w * r) ++ "px"
                    else
                        toString xOffset ++ "px " ++ toString yOffset ++ "px"

        flipAngle =
            case animation of
                Nothing ->
                    "0"

                Just a ->
                    "calc(" ++ toString (animate currentTick a) ++ "deg)"
    in
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

        --, rotate angle
        , Events.onClick (FlipCard id)
        , htmlAttribute <|
            style
                [ ( "background-image", "url(\"/assets/playing cards Merge.jpg\")" )
                , ( "background-position", spritePos )
                , ( "background-size", toString (w * 13) ++ "px " ++ toString (h * 5) ++ "px" )

                --, ( "transition", "transform 1s" )
                , ( "transform", "rotateY(" ++ flipAngle ++ ")" )
                ]
        ]
        none
