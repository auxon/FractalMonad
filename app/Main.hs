module Main where
import Clocks (initClock, incrementClock, window, background, drawState)
import Control.Monad.State (execState, replicateM)
import Graphics.Gloss (simulate)

-- The main function that sets up the Gloss window and rendering.
main :: IO ()
main = do
    let bases = [2, 3, 5, 7] -- For example, binary, ternary, quinary, septenary systems.
    let initialState = initClock bases
    -- Simulate the clock for a few steps to get an interesting initial state.
    let steps = 100
    let finalState = execState (replicateM steps incrementClock) initialState
    -- Run the Gloss application.
    simulate window background 1 finalState drawState (\_ _ -> execState incrementClock)
