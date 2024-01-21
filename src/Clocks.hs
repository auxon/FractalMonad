module Clocks (initClock, incrementClock, clockToPictures, window, background, drawState) where

import Control.Monad.State
import Graphics.Gloss (Picture, Display (InWindow), Color, white, makeColorI, arcSolid, translate, color, pictures)

-- Each hand is represented by its current value and its base.
type Hand = (Float, Float) -- (current value, base)
type ClockState = [Hand]

-- Initialize the clock with a list of bases for each hand.
initClock :: [Float] -> ClockState
initClock = map(0,) -- Start all hands at 0.

-- Increment the clock, starting with the least significant hand.
incrementClock :: State ClockState ()
incrementClock = do
    hands <- get
    let (newHands, _) = increment hands
    put newHands
  where
    increment [] = ([], 0) -- No carry if there are no hands.
    increment ((val, base):xs) =
        let (rest, carry) = increment xs
            newVal = val + carry
        in if newVal >= base
            then ((0, base) : rest, 1) -- Carry over to the next hand.
            else ((newVal, base) : xs, 0) -- No carry, just update the hand.

-- Convert the clock hands to a list of pictures for rendering.
clockToPictures :: ClockState -> [Picture]
clockToPictures = zipWith handToPicture [0..]
  where
    handToPicture index (val, base) =
        let angle = val / base * 360
            colorValue = makeColorI (255 - index * 30) (index * 30) 255 255
        in translate (200 * sin (fromIntegral index)) (200 * cos (fromIntegral index)) $
           color colorValue (arcSolid 0 angle 50)

-- The display window
window :: Display
window = InWindow "Fractal Clock" (600, 600) (10, 10)

-- The background color
background :: Color
background = white

-- The main drawing function
drawState :: ClockState -> Picture
drawState clockState = pictures $ clockToPictures clockState