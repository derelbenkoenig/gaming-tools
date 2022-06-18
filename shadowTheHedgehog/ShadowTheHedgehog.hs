{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module ShadowTheHedgehog where

import qualified Data.Map.Lazy as Map
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Control.Monad (replicateM_)

data Alignment = Dark | Neutral | Hero
    deriving (Read, Show, Enum, Eq, Ord)

data Level = Level {
    name :: String,
    paths :: Map.Map Alignment Level
} deriving (Eq)

instance Show Level where
    show Level {name, paths} =
        name
        ++ ", Paths: ["
        ++ (intercalate ", " $ map showPath $ Map.toAscList paths)
        ++ "]"
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

-- The Ark is unusual in having a Neutral and Dark instead of Hero and Dark, and the Neutral
-- mission sends you in the hero direction on the path.
theArk = mkLevel "The Ark" (Just gunFortress) (Just blackComet) Nothing
airFleet = mkLevel "Air Fleet" (Just gunFortress) (Just blackComet) (Just lavaShelter)
ironJungle = levelBelowWithHero airFleet "Iron Jungle" (Just cosmicFall)
spaceGadget = levelBelowWithHero ironJungle "Space Gadget" (Just finalHaunt)
-- Lost Impact is unusual in having no Dark mission, and even more so in that the Neutral mission
-- actually moves you in the Dark direction on the path. 
lostImpact = mkLevel "Lost Impact" Nothing (Just cosmicFall) (Just finalHaunt)

-- all of the "last" levels have only a Hero and Dark mission that simply end the story path
lastLevel name = mkLevel name (Just darkEnding) Nothing (Just heroEnding)

gunFortress = lastLevel "GUN Fortress"
blackComet = lastLevel "Black Comet"
lavaShelter = lastLevel "Lava Shelter"
cosmicFall = lastLevel "Cosmic Fall"
finalHaunt = lastLevel "Final Haunt"

darkEnding = pathlessLevel "Dark Ending"
heroEnding = pathlessLevel "Hero Ending"

data Choice = Choice Level Alignment
newtype ChoiceTo = ChoiceTo Choice

instance Show Choice where
    show (Choice Level{name,paths} align) = 
        name
        ++ ", "
        ++ show align
        ++ " mission"

destination (Choice Level{paths} align) = paths Map.! align

instance Show ChoiceTo where 
    -- throws a runtime error if you try to choose a path that level doesn't have
    show (ChoiceTo choice@(Choice Level{name,paths} align)) =
        show choice
        ++ " => "
        ++ show nextLevelName
            where Level{name=nextLevelName} = destination choice

choices level@Level{name,paths} = map (Choice level) (Map.keys paths)

newtype Route = Route [Choice]

instance Show Route where
    show (Route cs) = "[ " ++ intercalate " => " (map show cs) ++ " ]"

enumerateRoutes :: Level -> [Route]
enumerateRoutes lv@Level{paths} = let
    steps = map (\(align, dest) -> ((Choice lv align), dest)) (Map.toAscList paths)
    stepToRoutes (c, dest) = map (\(Route cs) -> Route (c:cs)) (enumerateRoutes dest)
    in if null steps then [Route []] else concatMap stepToRoutes steps

allRoutes = enumerateRoutes westopolis

routeNum n = allRoutes !! (n - 1)

missionsOfRoute (Route cs) = map (\(Choice _ align) -> align) cs
