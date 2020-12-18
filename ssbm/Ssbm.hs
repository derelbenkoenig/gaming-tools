module Ssbm where

shieldstun :: Double -> Double -> Int
shieldstun damage triggerLevel =
    floor $ (200.0 / 201.0) * (((d * (a + 0.3) * 1.5) + 2.0))
    where
        d = damage
        a = 0.65 * (1 - ((s - 0.3) / 0.7))
        s = triggerLevel
