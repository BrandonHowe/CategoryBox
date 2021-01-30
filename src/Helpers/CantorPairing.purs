module CategoryBox.Helpers.CantorPairing where

import Prelude

import Data.Int (floor, toNumber)
import Data.Tuple (Tuple(..))
import Math (sqrt)

-- | Bijective function that converts an integer into a pair of 2 integers (reverse of Cantor pairing)
invertCantorPairing :: Int -> Tuple Int Int
invertCantorPairing int = Tuple x y
  where
    -- | Don't ask how this works. I don't know.
    num = toNumber int
    t = toNumber $ floor $ (-1.0 + (sqrt $ 1.0 + 8.0 * num)) / 2.0
    x = floor $ t * (t + 3.0) / 2.0 - num
    y = floor $ num - t * (t + 1.0) / 2.0