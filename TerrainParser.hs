module TerrainParser where




import           Data.Char  (toLower)
import           Data.List  (elemIndex, intercalate)
import           Data.Maybe (fromJust, isJust)
import           GameDef    (Block, Board, Cell (..), Pos, Terrain, block,
                             makeBlock, makeTerrain, terrainBoard)
import           Utils      (addIndices, replace, replaceWith)





type ErrorMsg = String



readTerrain :: String -> Either ErrorMsg Terrain
readTerrain str = extractStart b >>= \s -> extractTarget b >>= \t -> readBoard b >>= \bo -> return $ makeTerrain (clearBoard bo) (startBlock s) t
    where b = lines str



showTerrain :: Terrain -> String
showTerrain = showBoard . terrainToBoard



terrainToBoard :: Terrain -> Board
terrainToBoard t = placeBlock (terrainBoard t) (block t)



placeBlock :: Board -> Block -> Board
placeBlock b (p1, p2) = replaceCell (replaceCell b p2 Occupied) p1 Occupied



replaceCell :: Board -> Pos -> Cell -> Board
replaceCell b (row, col) c = replaceWith row replaceRow b
    where replaceRow = replace col c



showBoard :: Board -> String
showBoard = intercalate "\n" . (map . map) showCell



showCell :: Cell -> Char
showCell c = case c of
              Occupied -> 'S'
              Target   -> 'T'
              Empty    -> 'o'
              Fragile  -> 'r'
              Illegal  -> '-'



extractStart :: [String] -> Either ErrorMsg Pos
extractStart = extract 'S'



extractTarget :: [String] -> Either ErrorMsg Pos
extractTarget = extract 'T'



extract :: Char -> [String] -> Either ErrorMsg Pos
extract ch s = case filter (\(_, i) -> isJust i) . addIndices . map find . lower $ s of
                []          -> Left $ "Error: essential character missing: " ++ [ch]
                ((i, x): _) -> Right (i, fromJust x)

    where lower = (map . map) toLower
          find  = elemIndex . toLower $ ch



startBlock :: Pos -> Block
startBlock p = makeBlock p p



readBoard :: [String] -> Either ErrorMsg Board
readBoard = (mapM . mapM) readCell



readCell :: Char -> Either ErrorMsg Cell
readCell ch = case toLower ch of
               's' -> Right Occupied
               'o' -> Right Empty
               'r' -> Right Fragile
               't' -> Right Target
               '-' -> Right Illegal
               _   -> Left $ "Error: illegal character " ++ [ch]



clearBoard :: Board -> Board
clearBoard = (map . map) removeS
    where removeS c = case c of
                        Occupied -> Empty
                        _        -> c
