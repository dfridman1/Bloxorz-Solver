module Bloxorz
       (
         bloxorz,
         bloxorzPath
       ) where



import           Data.List      (intercalate)
import           GameDef        (Block, Move, Terrain, block, change, makeBlock,
                                 targetPos, updateTerrain, validNeighbors)
import           Generalization (Graph, buildGraph)
import           TerrainParser  (readTerrain, showTerrain)



type Path = [Move]



type ErrorMsg = String



bloxorz :: String -> String
bloxorz str = case bloxorzHelper str of
               Left err  -> err
               Right val -> val



bloxorzHelper :: String -> Either ErrorMsg String
bloxorzHelper str = readTerrain str >>= \t -> shortestPath t >>= \p -> return . showTerrains . terrainSequence t $ p
    where showTerrains = intercalate "\n\n\n" . map showTerrain



terrainSequence :: Terrain -> Path -> [Terrain]
terrainSequence t ps = map (updateTerrain t) blocks
    where blocks = blockPositions (block t) ps



bloxorzPath :: String -> Either ErrorMsg Path
bloxorzPath str = readTerrain str >>= shortestPath



shortestPath :: Terrain -> Either ErrorMsg Path
shortestPath t | null paths = Left "Error: no solutions found"
               | otherwise  = Right . reverse . snd . head $ paths
    where paths = filter (targetAchieved t) . graph $ t




type State = (Block, Path)



graph :: Terrain -> Graph State
graph t = buildGraph f (neighborsWithHistory t) startState
    where startState = (block t, [])
          f          = fst



neighborsWithHistory :: Terrain -> State -> [State]
neighborsWithHistory t (bl, hist) = map (\(b, move) -> (b, move: hist)) . validNeighbors t $ bl



targetAchieved :: Terrain -> State -> Bool
targetAchieved t (bl, _) = bl == makeBlock (targetPos t) (targetPos t)



blockPositions :: Block -> Path -> [Block]
blockPositions b = scanl change b
