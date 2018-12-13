{-# LANGUAGE OverloadedStrings, TypeApplications, TemplateHaskell, ViewPatterns #-}
module Pure.Radar.Utils where

import Pure.Data.Txt as Txt

import Data.List as List

import Text.Printf

infixl 1 <&>
(<&>) = flip (<$>)

-- | Polar to cartesian conversion
p2c :: Double -> Double -> (Double,Double)
p2c angle distance = (x,y)
    where
        x = cos a * distance
        y = sin a * distance
        a = angle - pi / 2

points :: [(Double,Double)] -> Txt
points ps = Txt.concat $ List.intersperse " " $ ps <&> \(x,y) -> 
    let 
        convert = toTxt @String . printf "%.4f"
    in
        convert x <> "," <> convert y

radius :: (Double,Double) -> Txt
radius end = 
    let origin = (0,0) 
    in points [origin,end]

