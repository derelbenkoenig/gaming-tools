
module Formatting where

padWithTo pad n str =
    if len >= n then str else str ++ padding
        where
        padding = take remaining $ concat $ repeat pad
        len = length str
        remaining = n - len

padListWithToMaximum pad ss = map (padWithTo pad n) ss where
    n = maximum (map length ss)
