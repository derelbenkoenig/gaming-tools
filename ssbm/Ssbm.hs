module Ssbm where

-- Fully depressed trigger corresponds with s = 1.0, 
-- while the lightest possible analog press that still yields shield has s = 0.30714.
shieldstun :: Double -> Double -> Int
shieldstun damage triggerLevel =
    floor $ (200.0 / 201.0) * (((d * (a + 0.3) * 1.5) + 2.0))
    where
        d = damage
        a = 0.65 * (1 - ((s - 0.3) / 0.7))
        s = triggerLevel

-- percent is _after the hit_, which means the _staled_ damage of the move has been applied.
-- damage, however, is the _unscaled_ value.
-- Note that fixed knockback is _not_ this formula with scaling=0. It is a different formula
-- (which still depends on weight)
knockback :: Double -> Double -> Double -> Double -> Double -> Double
knockback baseKb scaling damage percent weight =
    scalableKb * scaling + baseKb where
        scalableKb = damageContribution * weightContribution * 1.4 + 18
        damageContribution = (percent / 10) + (percent * (fromIntegral $ floor damage) / 20)
        weightContribution = 200 / (weight + 100)

hitstunOfKnockback kb = floor (kb * 0.4)
