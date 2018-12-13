# pure-radar

A library for simple svg radar charts. Based upon [Lorenzo Spinelli's](https://github.com/Spyna/) [react-svg-radar-chart](https://github.com/Spyna/react-svg-radar-chart).

## Example

An example using [pure-theme](https://github.com/grumply/pure-theme) for some simple.

```haskell
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
import Pure hiding (Shape)
import Pure.Data.CSS
import Pure.Radar
import Pure.Theme
import Control.Monad

-- Selectors corresponding to the shape field accessors. These must implement 
-- Enum and Bounded to work with Pure instance and Show for the Default 
-- instance.
data Column = Alpha | Beta | Gamma deriving (Enum,Bounded,Show)

-- the type of data from which we can extract values of the above columns
data Shape = Shape 
    { alpha :: Double
    , beta :: Double
    , gamma :: Double
    }

-- tie the columns to the shape
point :: Column -> Shape -> Double
point Alpha = alpha
point Beta  = beta
point Gamma = gamma

-- a sample of shapes
shapes = [Shape 0.5 0.3 0.4, Shape 0.0 0.6 0.95]

main = inject body $ 
    Radar def
        <| Select point 
         . Dat shapes 
         . Scales 3 
         . LabelMargin 25
         . WithDot (\_ _ -> Theme DotT)
         . WithShape (\_ -> Theme ShapeT)

data DotT = DotT
instance Themeable DotT where
    theme c _ = void $ do

        is c .> do
            -- need to add svg styles to pure, apparently
            "fill"     =: lightgreen
            "r"        =: int 5
            transition =: "r" <<>> sec 0.2

        is c . is ":hover" .> do
            "fill" =: darkgreen
            "r"    =: int 8

data ShapeT = ShapeT
instance Themeable ShapeT where
    theme c _ = void $ do

        is c .>
            "fill-opacity" =: dec 0.3

        is c . is ":hover" .>
            "fill-opacity" =: dec 0.65
```