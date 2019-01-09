{-# LANGUAGE OverloadedStrings, TypeApplications, RecordWildCards, ScopedTypeVariables, ViewPatterns, TemplateHaskell, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, PatternSynonyms, ViewPatterns #-}
module Pure.Radar where

import Pure hiding (Name,features,properties)
import Pure.Data.CSS hiding (has)
import Pure.Data.SVG as SVG
import Pure.Data.SVG.Properties as SVG
import Pure.Data.Txt as Txt (concat)
import Pure.Data.Prop.TH
import Pure.Data.Prop
import Pure.Theme

import GHC.Generics

import Pure.Radar.Utils as Utils

import Data.List as List
import Data.Foldable
import Data.Traversable
import Data.Typeable

import Text.Printf

data Radar b a = Radar_
    { features :: Features
    , size :: Int
    , scales :: Int
    , labelMargin :: Int
    , dat :: [a]
    , select :: a -> b -> Double
    , label :: b -> Txt
    , draw :: [(Double,Double)] -> Txt
    , withDot :: b -> a -> View -> View
    , withRadius :: b -> View -> View
    , withCaption :: b -> View -> View
    , withScale :: Int -> View -> View
    , withShape :: a -> View -> View
    }

deriveLocalComponent ''Radar

instance Show b => Default (Radar b a) where
    def = Radar_ def 300 0 0 [] (\_ _ -> def) (toTxt . show) noSmoothing
            (\_ _ -> id) (\_ -> id) (\_ -> id) (\_ -> id) (\_ -> id)

instance (Typeable a, Typeable b, Enum b, Bounded b) => Pure (Radar b a) where
    view Radar_ {..} =
        let
            columns :: [b]
            columns = enumFrom (minBound :: b)

            pts :: [(a,[Double])]
            pts = dat <&> \d -> (d,columns <&> \c -> select d c)

            sz :: Double
            sz = fromIntegral size

            viewBox :: Txt
            viewBox = neg (int labelMargin)
                        <<>> zero
                        <<>> int (size + labelMargin * 2)
                        <<>> int size

            angles :: [Double]
            angles =
                let l = fromIntegral (List.length columns)
                in [1..l] <&> \i -> pi * 2 * i / l

            fixed :: Double -> Txt
            fixed = toTxt @String . printf "%.4f"

            polarize :: [Double] -> [(Double,Double)]
            polarize (zip angles -> ads) = ads <&> \(a,d) -> p2c a (d * sz / 2)

            translate :: Txt
            translate = "translate(" <> delta <> "," <> delta <> ")"
                where
                    delta = fixed (sz / 2)

            dots :: [(a,b,Double,Double)]
            dots = concatMap (\(a,ps) -> zip4 (repeat a) columns angles ps) pts

            radiiG :: View
            radiiG =
                G <||>
                    ( zip angles columns <&> \(a,c) ->
                        Polyline <| withRadius c . Stroke "gray"
                                  . Points (Utils.points [(0,0),p2c a (sz / 2)])
                    )

            shapeG :: View
            shapeG =
                G <||>
                    (pts <&> \(a,ps) ->
                        Path <| withShape a . Stroke "black"
                              . D (draw (polarize ps))
                    )

            scaleG:: View
            scaleG =
                G <||>
                    (List.reverse [1..scales] <&> \i@(fromIntegral -> s) ->
                        let r = s / fromIntegral scales * sz / 2
                        in Circle <| withScale i
                                   . Stroke "gray"
                                   . Fill "#fafafa"
                                   . Cx zero
                                   . Cy zero
                                   . R (toTxt r)
                    )

            captionG :: View
            captionG =
                G <||>
                    (zip angles columns <&> \(a,c) ->
                        let (x,y) = p2c a (sz / 2 * 0.95)
                        in SVG.Text <| withCaption c . X (fixed x) . Y (fixed y) . Dy "5" . TextAnchor "middle" |>
                            [ text (label c) ]
                    )

            dotG :: View
            dotG =
                G <||>
                    ( dots <&> \(a,b,angle,dist) ->
                        let (x,y) = p2c angle (dist * sz / 2)
                        in Circle <| withDot b a . Cx (fixed x) . Cy (fixed y)
                    )

        in
            Svg <| Width (int size) . Height (int size) . ViewBox viewBox |>
                [ G <| SVG.Transform translate|>
                    [ scaleG
                    , radiiG
                    , shapeG
                    , captionG
                    , dotG
                    ]
                ]

noSmoothing [] = ""
noSmoothing (p:ps) =
    let d2t = toTxt @ String . printf "%.4f"
        pt x y = d2t x <> "," <> d2t y
        m (x,y) = "M" <> pt x y
        l (x,y) = "L" <> pt x y
    in Txt.concat $ m p : fmap l ps ++ [ "z" ]
