{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module ShadowTheHedgehog where

import qualified Data.Map.Lazy as Map
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Control.Monad (replicateM_)

-- this helps me see what I'm doing in the repl
blanklines n = replicateM_ n (putStrLn "")

data Alignment = Dark | Neutral | Hero
    deriving (Read, Show, Enum, Eq, Ord)

data Level = Level {
    name :: String,
    paths :: Map.Map Alignment Level
} deriving (Eq)

instance Show Level where
    show Level {name, paths} =
        name ++
        ", Paths: [" ++
        (intercalate ", " $ map showPath $ Map.toAscList paths) ++
        "]"
            where showPath (align, Level {name}) = show align ++ " -> " ++ name

undefinedLevel = Level "Undefined" Map.empty
pathlessLevel name = Level {name, paths = Map.empty}

mkLevel :: String -> Maybe Level -> Maybe Level -> Maybe Level -> Level
mkLevel name darkPath neutralPath heroPath = Level name paths where
    pathList = [darkPath,neutralPath,heroPath]
    maybeEntries = zipWith (\a b -> (a,) <$> b) (enumFrom Dark) pathList
    paths = Map.fromAscList $ catMaybes $ maybeEntries

-- the levels are displayed left to right, with dark paths going up and hero paths going down.
-- in general, but with exceptions, the level "below" a certain level will have its
-- dark path be the above level's neutral path, and its neutral path be the above level's
-- hero path. 
-- The exception is that for levels at the top or bottom, some have a hero and a dark mission,
-- but others may have a hero and a neutral or a neutral and a dark instead, so it's not regular
-- at the edges. But this function can be used sometimes, at least if the "above" level is one
-- with all three missions

levelBelowWithHero (Level {name,paths}) newName newHeroPath =
    mkLevel
        newName
        (paths Map.!? Neutral) -- old neutral path == new dark path
        (paths Map.!? Hero) -- old hero path == new neutral path
        newHeroPath

-- declaration of all levels and their paths

westopolis = mkLevel "Westopolis" (Just digitalCircuit) (Just glyphicCanyon) (Just lethalHighway)

digitalCircuit = mkLevel "Digital Circuit" (Just crypticCastle) Nothing (Just prisonIsland)
glyphicCanyon = mkLevel "Glyphic Canyon" (Just crypticCastle) (Just prisonIsland) (Just circusPark)
lethalHighway = mkLevel "Lethal Highway" (Just prisonIsland) Nothing (Just circusPark)

crypticCastle = mkLevel "Cryptic Castle" (Just centralCity) (Just theDoom) (Just skyTroops)
prisonIsland = levelBelowWithHero crypticCastle "Prison Island" (Just madMatrix)
circusPark = levelBelowWithHero prisonIsland "Circus Park" (Just deathRuins)

centralCity = mkLevel "Central City" (Just theArk) Nothing (Just airFleet)
theDoom = mkLevel "The Doom" (Just theArk) (Just airFleet) (Just ironJungle)
skyTroops = levelBelowWithHero theDoom "Sky Troops" (Just spaceGadget)
madMatrix = levelBelowWithHero skyTroops "Mad Matrix" (Just lostImpact)
deathRuins = mkLevel "Death Ruins" (Just spaceGadget) Nothing (Just lostImpact)

theArk = pathlessLevel "The Ark"
airFleet = pathlessLevel "Air Fleet"
ironJungle = pathlessLevel "Iron Jungle"
spaceGadget = pathlessLevel "Space Gadget"
lostImpact = pathlessLevel "Lost Impact"

gunFortress = pathlessLevel "GUN Fortress"
blackComet = pathlessLevel "Black Comet"
lavaShelter = pathlessLevel "Lava Shelter"
cosmicFall = pathlessLevel "Cosmic Fall"
finalHaunt = pathlessLevel "Final Haunt"
