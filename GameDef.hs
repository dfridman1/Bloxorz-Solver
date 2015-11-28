module GameDef
       (
         validNeighbors,
         standing,
         Terrain,
         Pos,
         Cell(..),
         Board,
         Block,
         Move,
         targetPos,
         block,
         makeBlock,
         change,
         terrainBoard,
         updateTerrain,
         makeTerrain
       ) where



import           Prelude hiding (Left, Right)
import           Utils   (atIndex)



type Pos   = (Int, Int)
type Block = (Pos, Pos)


data Cell  = Illegal | Empty | Fragile | Occupied | Target deriving Show
type Board = [[Cell]]


data Terrain = Terrain Board Block Pos deriving Show


data Move = Left | Right | Up | Down deriving Show



validNeighbors :: Terrain -> Block -> [(Block, Move)]
validNeighbors t = filter (\(bl, _) -> valid t bl) . neighbors



neighbors :: Block -> [(Block, Move)]
neighbors b = [(left b, Left),
               (right b, Right),
               (up b, Up),
               (down b, Down)]



left :: Block -> Block
left b = blockDy b . leftDir $ b



right :: Block -> Block
right b = blockDy b . rightDir $ b



up :: Block -> Block
up b = blockDx b . upDir $ b



down :: Block -> Block
down b = blockDx b . downDir $ b



blockDx :: Block -> (Int, Int) -> Block
blockDx (p1, p2) (dx1, dx2) = (p1 `posDx` dx1, p2 `posDx` dx2)



blockDy :: Block -> (Int, Int) -> Block
blockDy (p1, p2) (dy1, dy2) = (p1 `posDy` dy1, p2 `posDy` dy2)



posDx :: Pos -> Int -> Pos
posDx (x, y) dx = (x + dx, y)



posDy :: Pos -> Int -> Pos
posDy (x, y) dy = (x, y + dy)



leftDir :: Block -> (Int, Int)
leftDir b | standing b   = (-2, -1)
          | horizontal b = (-1, -2)
          | otherwise    = (-1, -1)



rightDir :: Block -> (Int, Int)
rightDir = swap . reflect . leftDir



upDir :: Block -> (Int, Int)
upDir b | standing b   = (-2, -1)
        | horizontal b = (-1, -1)
        | otherwise    = (-1, -2)



downDir :: Block -> (Int, Int)
downDir = swap . reflect . upDir



swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)



reflect :: Num a => (a, a) -> (a, a)
reflect (x, y) = (-x, -y)



valid :: Terrain -> Block -> Bool
valid t b@(p1, p2) = (not $ standingOnFragile board b) &&
                     terrain t p1 &&
                     terrain t p2
    where board = terrainBoard t



standingOnFragile :: Board -> Block -> Bool
standingOnFragile b bl@(pos, _) | not $ standing bl = False
                                | otherwise         = case cellAtPos b pos of
                                                       Just c  -> cellFragile c
                                                       Nothing -> False



terrain :: Terrain -> Pos -> Bool
terrain t p = case cellAtPos (terrainBoard t) p of
               Just c  -> legalCell c
               Nothing -> False



cellAtPos :: Board -> Pos -> Maybe Cell
cellAtPos b (row, col) = atIndex b row >>= (flip atIndex) col



legalCell :: Cell -> Bool
legalCell c = case c of
               Illegal -> False
               _       -> True


cellFragile :: Cell -> Bool
cellFragile c = case c of
                 Fragile -> True
                 _       -> False


standing :: Block -> Bool
standing (p1, p2) = p1 == p2



horizontal :: Block -> Bool
horizontal ((x1, y1), (x2, y2)) = (x1 == x2) && abs (y1 - y2) == 1



targetPos :: Terrain -> Pos
targetPos (Terrain _ _ t) = t



block :: Terrain -> Block
block (Terrain _ b _) = b



terrainBoard :: Terrain -> Board
terrainBoard (Terrain b _ _) = b



makeTerrain :: Board -> Block -> Pos -> Terrain
makeTerrain b bl = Terrain b bl



makeBlock :: Pos -> Pos -> Block
makeBlock p1 p2 = (p1, p2)



updateTerrain :: Terrain -> Block -> Terrain
updateTerrain (Terrain b _ t) bl = Terrain b bl t



change :: Block -> Move -> Block
change b move = case move of
                 Left  -> left b
                 Right -> right b
                 Up    -> up b
                 Down  -> down b
